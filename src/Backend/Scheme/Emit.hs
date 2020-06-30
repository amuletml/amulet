{-# LANGUAGE
  OverloadedStrings, NamedFieldPuns, FlexibleContexts
, FlexibleInstances, TupleSections, ViewPatterns
, ScopedTypeVariables, TemplateHaskell, QuasiQuotes
, TypeFamilies #-}
module Backend.Scheme.Emit
  (
  ) where


import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Applicative
import Control.Lens

import qualified Data.Sequence as Seq
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import Data.Sequence (Seq((:<|)))
import qualified Data.Text as T
import Data.Traversable
import Data.Position
import Data.Foldable
import Data.Triple
import Data.List (partition)

import qualified Core.Builtin as B
import Core.Occurrence
import Core.Intrinsic
import Core.Arity
import Core.Types
import Core.Core
import Core.Var

import Language.Scheme

import Backend.EmitGraph
import Backend.Lua.Builtin
import Backend.Lua.Inline
import Backend.Escape

data VarDecl
     -- ^ A foreign function whose body should be inlined when used.
  = VarInline Int ([Scheme] -> (Seq Scheme, [Scheme]))
    -- | A "normal" toplevel declaration, which should just be referenced
    -- by variable.
  | VarUpvalue

instance Show VarDecl where
  show (VarInline n f) =
    let e = f $ map (ScRef . ScName . T.pack . ("arg"++) . show) [1..n]
    in "VarInline(" ++ show e  ++ ")"
  show VarUpvalue = "VarUpvalue"

data Schm

-- | A wrapper for "simple" expressions
--
-- Purely exists so we don't accidentally start putting random things
-- into emitted expressions.
--
-- One should use 'simpleOf' instead of using the 'ScSimp' constructor,
-- for that additional safety.
newtype SchemeSimple = ScSimp { unsimple :: Scheme } deriving Show

-- | Controls how one of more variables should be returned from a
-- statement
data EmitYield
  = YieldReturn -- ^ Return the expression(s)
  | YieldDiscard -- ^ Discard this expression
  | YieldStore [SchemeName] -- ^ Assign this expression(s) to these variables
  | YieldDeclare CoVar Type Occurrence -- ^ Declare this variable
  deriving Show

-- | The top-level emitting state. This is substantially simpler than the
-- more general 'EmitState' as it need not maintain an expression graph.
data TopEmitState = TopEmitState
  { _topVars   :: VarMap.Map (VarDecl, [SchemeSimple])
  , _topArity  :: ArityScope
  , _topEscape :: EscapeScope
  , _topExVars :: ExtraVars
  }
  deriving Show

makeLenses ''TopEmitState

instance Emitter Schm where
  type EUpvalue Schm = VarDecl
  type EAtom    Schm = SchemeSimple
  type EExpr    Schm = Scheme
  type EStmt    Schm = SchemeStmt
  type EYield   Schm = EmitYield

  upvalue = VarUpvalue
  atomToExpr = unsimple
  mkExprRef = ScRef . ScName
  mkAtomRef = ScSimp . ScRef . ScName

  -- | Emit a declaration for a variable and a collection of expressions
  --
  -- This returns a 'ScLet' statement binding such expressions and the
  -- variables which were bound. We do not emit bindings for expressions
  -- which are just variables, returning variable directly.
  genDeclare escape v ty es
    -- If all expressions are simple, then just return them
    | Just vals <- traverse simpleOf es = pure (mempty, vals)
    -- Unpack tuples into multiple assignments
    | ValuesTy ts <- ty = do
        v' <- escape v
        let (rs, lhs, rhs) = tupleVars v' 1 ts es
        pure (Within (pure . ScLet (zipWith (\a b -> (a, [b])) lhs rhs) . toList), rs)
    -- Just do a normal binding
    | otherwise = do
        v' <- escape v
        let var = ScName v'
        pure (Within (pure . ScLet [(var, es)] . toList), [simpleVar var])

    where
      -- | Generate a set of bindings for a tuple variable
      tupleVars :: T.Text -> Int -> [Type] -> [Scheme]
              -> ([SchemeSimple], [SchemeName], [Scheme])
      tupleVars _ _ [] es = ([], [], es)
      tupleVars v n (_:ts) [] =
        let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
            var = ScName (v <> T.pack ('_':show n))
        in (simpleVar var : rs, var : lhs, rhs)
      tupleVars v n (_:ts) (e:es)
        | Just e' <- simpleOf e =
          let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
          in (e' : rs, lhs, rhs)
        | otherwise =
          let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
              var = ScName (v <> T.pack ('_':show n))
          in (simpleVar var : rs, var : lhs, e : rhs)

  -- | Generate the appropriate code for the provided yield.
  genYield _ YieldReturn es = pure (Leaf (packValues es), [])
  genYield _ YieldDiscard es = pure (Leaf (foldMap asBlock es), [])
  genYield _ (YieldStore vs) es = pure (Leaf (Seq.fromList (setVars vs es)), simpleVars vs)
  genYield e (YieldDeclare v ty _) es = genDeclare e v ty es

  -- | Convert an expression and a yield context to a set of statements.
  --
  -- Note, this assumes that the result of the expression is not directly
  -- used, and so 'YieldDeclare's will be treated like 'YieldDiscard'.
  yieldStmt YieldReturn es = Leaf (packValues es)
  yieldStmt (YieldStore vs) es = Leaf (Seq.fromList (setVars vs es))
  yieldStmt YieldDiscard es = Leaf (foldMap asBlock es)
  yieldStmt (YieldDeclare _ _ _) es = Leaf (foldMap asBlock es)

  yieldDeclare (YieldDeclare v ty occ) = Just (v, ty, occ)
  yieldDeclare _ = Nothing

-- | Emit a single expression within the current context
--
-- This walks down the expression spine (lets) until it finds
-- some leaf expression. These are compiled based on the current graph,
-- their dependencies analysed, and they are then inserted into the
-- expression/statement graph.
--
-- Most of the code for that is performed using 'withinExpr', but some
-- non-trivial terms (record extensions, matches) roll their own as we
-- generate statements instead.
emitExpr :: forall a m. ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState Schm) m )
         => a -> EmitYield -> AnnTerm VarSet.Set a
         -> m ()

emitExpr var yield (AnnLet _ (One (v, ty, e)) r) = do
  let yield' = if usedWhen v == Dead then YieldDiscard else YieldDeclare (toVar v) ty (usedWhen v)
  emitExpr v yield' e
  local (emitArity %~ flip extendPureLets [(v, ty, e)]) $
    emitExpr var yield r

-- Trivial terms. These will just be emitted inline.
emitExpr var yield t@(AnnAtom _ x)    = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnTyApp _ x _) = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnCast _ x _ _)  = withinExpr var yield t $ emitAtom x

emitExpr var yield t@(AnnApp _ f e) = withinTerm var t $ do
  e' <- emitAtom e
  f' <- emitAtomS f

  state <- use nodeState
  case f' of
    -- Attempt to inline native definitions and binary operators
    ScRef (ScName name)
      | Just name' <- getEscaped name (state ^. emitEscape)
      , Just EmittedUpvalue { emitTop = VarInline a b } <- state ^. (emitGraph . at name')
      , length e' == a -> case b e' of
          (Seq.Empty, es) -> pure $ EmittedExpr es yield
          (ss, es) -> do
            (ss', vals) <- genYield pushScope' yield es
            pure $ EmittedStmt (ss <> ss') vals
    _ -> pure $ EmittedExpr [ScCall f' e'] yield

emitExpr var yield (AnnLam _ TypeArgument{} b) = emitExpr var yield b

emitExpr var yield t@(AnnLam fv (TermArgument v ty) e) = do
  scope <- ask
  graph <- liftedGraph fv
  escape <- use emitEscape

  let (vs, escape') = flip runState escape $ genVars (state . pushVar) v ty (Just e)
      graph' = VarMap.insert (toVar v) (EmittedUpvalue VarUpvalue (simpleVars vs) (one v)) graph

      term :: [Scheme] = pure . ScLambda vs . toList . fst3 $ emitTerm scope escape' graph' YieldReturn e
  withinExpr var yield t (pure term)

emitExpr var yield t@(AnnValues _ xs) =
  {-
    There really isn't much we can do as far as unboxed tuples go. We
    just build up a list of all dependent expressions and statement.
  -}
  withinExpr var yield t $
    foldrM (\e es -> (:es) <$> emitAtomS e) mempty xs

emitExpr var yield t@(AnnExtend _ (Lit RecNil) fs) = do
  {-
    Record literals are nice and simple to generate, and can just be
    emitted as expressions.
  -}
  graph <- use emitGraph
  withinExpr var yield t $
    pure . LuaTable <$> foldrM emitRow mempty (sortAtoms graph thd3 fs)
  where emitRow (f, _, e) es = (:es) . (LuaString f,) <$> emitAtomS e


simpleVar :: SchemeName -> SchemeSimple
simpleVar = ScSimp . ScRef

simpleVars :: [SchemeName] -> [SchemeSimple]
simpleVars = map simpleVar

simpleOf :: Scheme -> Maybe SchemeSimple
simpleOf x | check x = Just (ScSimp x) where
  -- Small literals are simple.
  check ScNumber{} = True
  check ScBoolean{} = True
  check (ScString x) | T.length x <= 16 = True
  -- For the sake of argument, references to variables are simple
  check ScRef{} = True
  check _ = False
simpleOf _ = Nothing

packValues :: Alternative a => [Scheme] -> a Scheme
packValues [] = empty
packValues [e] = pure e
packValues es = pure (ScCall (ScRef (ScName "values")) es)


-- | Mutate a variable.
setVar :: SchemeName -> Scheme -> Scheme
setVar v e = ScCall (ScRef (ScName "set!")) [ScRef v, e]

-- | Mutate multiple variables.
setVars :: [SchemeName] -> [Scheme] -> [Scheme]
setVars = zipWith setVar

-- | Convert an expression into a set of Scheme statements
asBlock :: Scheme -> Seq Scheme
asBlock ScRef{} = mempty
asBlock ScString{} = mempty
asBlock ScNumber{} = mempty
asBlock ScBoolean{} = mempty
asBlock ScLambda{} = mempty
asBlock x@ScCall{} = pure x
asBlock x@ScIf{} = pure x
asBlock x@ScCond{} = pure x
asBlock x@ScLet{} = pure x

opOfIntrinsic :: Intrinsic -> T.Text
opOfIntrinsic IntAdd = "+"
opOfIntrinsic IntSub = "-"
opOfIntrinsic IntMul = "*"
opOfIntrinsic IntDiv = "/"
opOfIntrinsic IntPow = "^"
opOfIntrinsic IntEq  = "="
opOfIntrinsic IntLt  = "<"
opOfIntrinsic IntLe  = "<="

opOfIntrinsic FloatAdd = "+"
opOfIntrinsic FloatSub = "-"
opOfIntrinsic FloatMul = "*"
opOfIntrinsic FloatDiv = "/"
opOfIntrinsic FloatPow = "^"
opOfIntrinsic FloatEq  = "="
opOfIntrinsic FloatLt  = "<"
opOfIntrinsic FloatLe  = "<="

opOfIntrinsic StrConcat = "string-append"
opOfIntrinsic StrEq = "string=?"
opOfIntrinsic StrLt = "string<?"
opOfIntrinsic StrLe = "string<=?"

opOfIntrinsic BoolEq = "eq?"
