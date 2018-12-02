{-# LANGUAGE
  OverloadedStrings, NamedFieldPuns, FlexibleContexts
, TupleSections, ViewPatterns, ScopedTypeVariables
, TemplateHaskell, QuasiQuotes #-}
module Backend.Lua.Emit
  ( emitStmt
  , TopEmitState(..), topVars, topArity, topEscape, topExVars
  , LuaSimple, unsimple
  , defaultEmitState
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import qualified Data.Sequence as Seq
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map as Map
import Data.Sequence (Seq((:<|)))
import qualified Data.Text as T
import Data.Traversable
import Data.Bifunctor
import Data.Foldable
import Data.Triple
import Data.List (partition)

import Core.Occurrence
import Core.Arity
import Core.Types
import Core.Core
import Core.Var

import Language.Lua.Parser
import Language.Lua.Syntax
import Language.Lua.Quote

import Backend.Lua.Builtin
import Backend.Lua.Inline
import Backend.Escape

-- | A magic variable used to represent the return value
vReturn :: CoVar
vReturn = CoVar (-100) "<<ret>>" ValueVar

data VarDecl
     -- ^ A foreign function whose body should be inlined when used.
  = VarInline Int ([LuaExpr] -> (Seq LuaStmt, [LuaExpr]))
    -- | A "normal" toplevel declaration, which should just be referenced
    -- by variable.
  | VarUpvalue

instance Show VarDecl where
  show (VarInline n f) =
    let e = f $ map (LuaRef . LuaName . T.pack . ("arg"++) . show) [1..n]
    in "VarInline(" ++ show e  ++ ")"
  show VarUpvalue = "VarUpvalue"

-- | A wrapper for "simple" Lua expressions
--
-- Purely exists so we don't accidentally start putting random things
-- into 'EmittedStmt's.
--
-- One should use 'simpleOf' instead of using the 'LuaSimp' constructor,
-- for that additional safety.
newtype LuaSimple = LuaSimp { unsimple :: LuaExpr }
  deriving Show

-- | A node in the  graph of emitted expressions and statements
--
-- 'EmittedExpr's should be merged inline into another expression if
-- doing so does not introduce loops into the graph. Meanwhile one should
-- consume the 'EmittedStmt' 'emitBound' variables from the statement
-- instead.
data EmittedNode a
  = EmittedExpr
    { emitExprs  :: [LuaExpr]   -- ^ The expression(s) which will be emitted
    , emitYield  :: EmitYield a -- ^ The result we should be binding this to if consumed
    , _emitBinds :: [a]         -- ^ The variables bound in the statement. Used when traversing the dependency
                                -- graph.
    , _emitDeps  :: VarSet.Set  -- ^ The dependencies for this node. Consuming it should assimilate them.
    }
  | EmittedStmt
    { emitStmts  :: Seq LuaStmt -- ^ The statements required before this can be evaluated
    , emitVals   :: [LuaSimple] -- ^ Each value or variable declared by this node.
    , _emitBinds :: [a]         -- ^ The variables bound in the statement. Used when traversing the dependency
                                -- graph.
    , _emitDeps  :: VarSet.Set  -- ^ The dependencies for this node. Consuming need not assimilate them.
    }
  | EmittedUpvalue
    { emitTop    :: VarDecl     -- ^ The top level declaration to consume
    , emitVals   :: [LuaSimple] -- ^ Each value or variable declared by this node.
    , _emitBinds :: [a]         -- ^ The variables bound in the statement. Used when traversing the dependency graph.
    }
  deriving Show

-- | The graph of all 'EmittedNode's.
type EmittedGraph a = VarMap.Map (EmittedNode a)

-- | The top-level emitting state. This is substantially simpler than the
-- more general 'EmitState' as it need not maintain an expression graph.
data TopEmitState = TopEmitState
  { _topVars   :: VarMap.Map (VarDecl, [LuaSimple])
  , _topArity  :: ArityScope
  , _topEscape :: EscapeScope
  , _topExVars :: ExtraVars
  }
  deriving Show

-- | The current state for the expression/term emitter. This is thread
-- through a 'MonadState' instance.
data EmitState a = EmitState
  { _emitGraph  :: EmittedGraph a
  , _emitPrev   :: VarSet.Set
  , _emitEscape :: EscapeScope
  }
  deriving Show

-- | The current scope for the expression/term emitter. This is thread
-- through a 'MonadReader' instance.
newtype EmitScope a = EmitScope
  { _emitArity :: ArityScope
  }
  deriving Show

-- | Controls how one of more variables should be returned from a
-- statement
data EmitYield a
  = YieldReturn -- ^ Return the expression(s) using 'LuaReturn'
  | YieldDiscard -- ^ Discard this expression
  | YieldStore [LuaVar] -- ^ Assign this expression(s) to these variables
  | YieldDeclare a (Type a) -- ^ Declare this variable
  deriving Show

-- | The state for emitting a single node.
--
-- This is a wrapper for 'EmitState', but also tracking dependencies for
-- this node. One generally uses 'runNES' in order to evaluate this.
data NodeEmitState a = NES
  { _nodeState :: EmitState a
  , _nodeDeps  :: VarSet.Set
  }

makeLenses ''EmittedNode
makeLenses ''TopEmitState
makeLenses ''EmitState
makeLenses ''EmitScope
makeLenses ''NodeEmitState

-- | The default (initial) state for the emitter.
defaultEmitState :: TopEmitState
defaultEmitState
  = TopEmitState
      (VarMap.map (bimap (uncurry VarInline) (pure . simpleVar)) builtinBuilders)
      emptyScope builtinEscape builtinVars

liftedGraph :: forall a m.
               ( IsVar a
               , MonadReader (EmitScope a) m
               , MonadState (EmitState a) m )
            => VarSet.Set -> m (EmittedGraph a)
liftedGraph = VarSet.foldr liftNode (pure mempty) where
  liftNode :: CoVar -> m (EmittedGraph a) -> m (EmittedGraph a)
  liftNode v m = do
    existing <- uses emitGraph (VarMap.lookup v)
    case existing of
      Just n@EmittedUpvalue{} -> VarMap.insert v n <$> m
      _ -> do
        n <- emitVarBinds v
        VarMap.insert v (EmittedUpvalue VarUpvalue n [fromVar v]) <$> m

-- | A wrapper for 'emitLiftedES' which converts the result to a sequence
-- of 'LuaStmt's.
emitLifted :: ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState a) m )
         => EmitYield a -> AnnTerm VarSet.Set a
         -> m (Seq LuaStmt)
emitLifted yield term = do
  graph <- liftedGraph (extractAnn term)
  scope <- ask
  escape <- use emitEscape
  pure . fst3 $ emitTerm scope escape graph yield term

-- | Emit an term within a child scope.
--
-- This will not modify the existing scope/state in any way, it's just a
-- convenience function to avoid having to extract values manually.
emitLiftedES :: ( Occurs a
                , MonadReader (EmitScope a) m
                , MonadState (EmitState a) m )
             => EmitYield a -> AnnTerm VarSet.Set a
             -> m (Either [LuaExpr] (Seq LuaStmt))
emitLiftedES yield term = do
  graph <- liftedGraph (extractAnn term)
  scope <- ask
  escape <- use emitEscape
  pure $ emitTermES scope escape graph yield term

-- | A wrapper for 'emitTermES' which converts the result to a sequence
-- of 'LuaStmt's.
emitTerm :: forall a. Occurs a
         => EmitScope a -> EscapeScope -> EmittedGraph a
         -> EmitYield a -> AnnTerm VarSet.Set a
         -> (Seq LuaStmt, [LuaSimple], EscapeScope)
emitTerm scope esc vars yield term =
  let (vals, EmitState graph prev escapes)
        = flip runReader scope
        . flip runStateT EmitState
          { _emitGraph  = vars
          , _emitPrev   = mempty
          , _emitEscape = esc
          }
        $ emitExpr (fromVar vReturn) yield term >> emitVarBinds (fromVar vReturn)

  in ( fst $ flushGraph vReturn prev graph
     , vals
     , escapes )

-- | Emit a term within the provided context.
--
-- This builds up a graph of expressions/statements, then performs a
-- topological sort on the resulting nodes.
emitTermES :: forall a. Occurs a
           => EmitScope a -> EscapeScope -> EmittedGraph a
           -> EmitYield a -> AnnTerm VarSet.Set a
           -> Either [LuaExpr] (Seq LuaStmt)
emitTermES scope esc vars yield term =
  let (_, EmitState graph prev _)
        = flip runReader scope
        . flip runStateT EmitState
          { _emitGraph  = vars
          , _emitPrev   = mempty
          , _emitEscape = esc
          }
        $ emitExpr (fromVar vReturn) yield term

      Just retNode = VarMap.lookup vReturn graph
      (stmts, _) = flushDeps retNode prev graph

  in case (stmts, retNode) of
       (Seq.Empty, EmittedExpr { emitExprs = es }) -> Left es
       _ -> Right (stmts <> nodeStmts retNode)

-- | Worker function to remove a node from a graph and emit all its
-- dependencies, returning the sequence of statements and the modified
-- graph.
--
-- This is used in 'emitTerm' and 'emitTermES'.
flushGraph :: IsVar a => CoVar -> VarSet.Set -> EmittedGraph a
           -> (Seq LuaStmt, EmittedGraph a)
flushGraph var extra g =
  case VarMap.lookup var g of
    Nothing -> (mempty, g)
    Just node -> first (<>nodeStmts node) (flushDeps node extra g)

-- | Worker function to remove all of a node's dependencies and emit
-- them, returning the statements and the modified graph.
--
-- This is used by 'emitTerm' and 'emitTermES'
flushDeps :: IsVar a => EmittedNode a -> VarSet.Set -> EmittedGraph a
          -> (Seq LuaStmt, EmittedGraph a)
flushDeps node extra g =
  let
    -- Remove this from the graph
    g' = foldr (VarMap.delete . toVar) g (node ^. emitBinds)
  in VarSet.foldr
     (\v (s, g) -> first (s<>) (flushGraph v mempty g))
     (mempty, g') (node ^. emitDeps <> extra)

-- | Extract the statements from this node.
nodeStmts :: EmittedNode a -> Seq LuaStmt
nodeStmts EmittedStmt { emitStmts = s } = s
nodeStmts EmittedExpr { emitExprs = es, emitYield = yield } = yieldStmt yield es
nodeStmts EmittedUpvalue{} = mempty

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
            , MonadState (EmitState a) m )
         => a -> EmitYield a -> AnnTerm VarSet.Set a
         -> m ()

emitExpr var yield (AnnLet _ (One (v, ty, e)) r) = do
  let yield' = if usedWhen v == Dead then YieldDiscard else YieldDeclare v ty
  emitExpr v yield' e
  local (emitArity %~ flip extendPureLets [(v, ty, e)]) $
    emitExpr var yield r

emitExpr var yield (AnnLet _ (Many vs) r) =
  local (emitArity %~ flip extendPureLets vs) $ do
    binds <- traverse (\(v, ty, _) -> (v,) <$> genVars pushScope v ty (Just r)) vs
    stmt <- traverse (\((_, _, e), (_, v')) -> emitLifted (YieldStore v') e) (zip vs binds)

    graph <- use emitGraph
    prev  <- use emitPrev
    arity <- view emitArity
    let stmts = case mconcat stmt of
                  (LuaAssign [v] [LuaFunction args bod] :<| Empty) -> pure (LuaLocalFun v args bod)
                  stmts -> LuaLocal (foldMap snd binds) [] <| stmts
        p = all (isPure arity . thd3) vs
        vss = VarSet.fromList (map (toVar . fst3) vs)

        -- We depend on the previous node (if impure), all free variables and
        -- all other nodes within the binding group.
        deps = (if p then mempty else prev)
               <> VarSet.filter (`VarMap.member` graph) (foldMap (extractAnn . thd3) vs)
               `VarSet.difference` vss

        -- Add us to every node
        vs' = map fst3 vs
        graph' = foldr (\(v, v') -> VarMap.insert (toVar v) (EmittedStmt stmts (simpleVars v') vs' deps)) graph binds

    -- Update the graph and impure set if needed
    emitGraph .= graph'
    emitPrev .= if p then prev else vss

    -- Emit the body within the existing context
    emitExpr var yield r

emitExpr var yield (AnnMatch _ test [arm@Arm { _armPtrn = p, _armBody = body, _armVars = vs }])
  | [(v, _)] <- filter (doesItOccur . fst) vs
  , usedWhen v == Once
  = do
      -- Push our pattern into the scope
      withinExpr v (YieldDeclare v (getTy v)) (AnnAtom (freeInAtom test) test) $ do
        test' <- emitAtom test
        pure . snd . head $ patternBindings p test'

      -- Emit the body within the existing context
      emitExpr var yield body

  | otherwise = do
    test' <- emitAtomMany test
    -- Push those patterns into the graph
    graph' <- patternGraph test test' arm =<< use emitGraph
    emitGraph .= graph'

    -- Emit the body within the existing context
    emitExpr var yield body

  where getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

emitExpr var yield t@(AnnMatch _ test arms) = do
  (yield', stmt, bound) <- case yield of
    YieldReturn -> pure (YieldReturn, mempty, mempty)
    YieldDiscard -> pure (YieldDiscard, mempty, mempty)
    YieldStore vs -> pure (YieldStore vs, mempty, vs)
    YieldDeclare v ty -> do
      v' <- genVars pushScope v ty (Just t)
      pure (YieldStore v', pure (LuaLocal v' []), v')

  (deps, body) <- case arms of
    {-
      Whilst this may seem a little weird special casing this, as we'll
      generate near equivalent code in the general case, it does allow us to
      merge the test with the definition, as we know it'll only be evaluated
      once.
    -}
    (Arm { _armPtrn = PatLit LitTrue, _armBody = ifs }:arms')
      | Just Arm { _armBody = els } <- find ((/=PatLit LitTrue) . _armPtrn) arms'
      -> genIf yield' ifs els
    (Arm { _armPtrn = PatLit LitFalse, _armBody = els }:arms')
      | Just Arm { _armBody = ifs } <- find ((/=PatLit LitFalse) . _armPtrn) arms'
      -> genIf yield' ifs els

    _ -> do
      {-
        In the case of the general branch, we will probably be consuming the
        pattern multiple times, and so we need to emit it as a variable.
      -}
      test' <- emitAtomMany test
      (freeInAtom test,) . Right . pure . LuaIfElse <$> for arms (genBranch test' yield')

  let node = case body of
        Left es -> EmittedExpr [es] yield
        Right ss -> EmittedStmt (stmt <> ss) (simpleVars bound)

  -- Add this node to the graph, depending on the arm body, and the previous impure node.
  prev <- use emitPrev
  emitGraph %= VarMap.insert (toVar var)
    (node [var]
          (deps <> foldMap (extractAnn . _armBody) arms <> prev))
    -- For the time being, we just assume this expression is impure.
  emitPrev .= VarSet.singleton (toVar var)

  where
    genBranch test' yield' arm@Arm { _armPtrn = p, _armBody = b } = do
      graph <- patternGraph test test' arm =<< liftedGraph (extractAnn b)
      scope <- ask
      escape <- use emitEscape

      let b' = fst3 $ emitTerm scope escape graph yield' b
      pure (patternTest escape p test', toList b')

    genIf :: EmitYield a
          -> AnnTerm VarSet.Set a -> AnnTerm VarSet.Set a
          -> m (VarSet.Set, Either LuaExpr (Seq LuaStmt))
    genIf yield ifs els = do
      (deps, test') <- runNES (freeInAtom test) (emitAtomS test)

      ifs' <- emitLiftedES yield ifs
      els' <- emitLiftedES yield els
      pure . (deps,) $ case (ifs', els') of
        -- Handle binary ops. Note we don't need to ensure e is not an
        -- unboxed tuple, as we know this returns a boolean.
        (Left [LuaTrue], Left [e]) -> Left $ LuaBinOp test' "or" e
        (Left [e], Left [LuaFalse]) -> Left $ LuaBinOp test' "and" e
        (Left [LuaFalse], Left [LuaTrue]) -> Left $ negate test'
        -- If we return a single expression, try to generate some
        -- slightly nicer code.
        (Left{}, _) | YieldReturn <- yield -> Right . Seq.fromList $
          LuaIfElse [(test', eitherStmts yield ifs')] : eitherStmts yield els'
        (_, Left{}) | YieldReturn <- yield -> Right . Seq.fromList $
          LuaIfElse [(negate test', eitherStmts yield els')] : eitherStmts yield ifs'

        _ ->
          Right . pure $ case (eitherStmts yield ifs', eitherStmts yield els') of
            (ifs'', Empty) -> LuaIfElse [(test', ifs'')]
            (Empty, els'') -> LuaIfElse [(negate test', els'')]
            (ifs'', els'') -> LuaIf test' ifs'' els''

    negate :: LuaExpr -> LuaExpr
    negate (LuaUnOp "not" x) = x
    negate (LuaBinOp a "==" b) = LuaBinOp a "~=" b
    negate (LuaBinOp a "~=" b) = LuaBinOp a "==" b
    negate (LuaBinOp a "and" b) = LuaBinOp (negate a) "or" (negate b)
    negate (LuaBinOp a "or" b) = LuaBinOp (negate a) "and" (negate b)
    negate x = LuaUnOp "not" x

    eitherStmts _ (Right ss) = toList ss
    eitherStmts yield (Left es) = toList $ yieldStmt yield es

-- Trivial terms. These will just be emitted inline.
emitExpr var yield t@(AnnAtom _ x)    = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnTyApp _ x _) = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnCast _ x _)  = withinExpr var yield t $ emitAtom x

emitExpr var yield t@(AnnApp _ f e) = withinTerm var t $ do
  e' <- emitAtom e
  f' <- emitAtomS f

  state <- use nodeState
  case f' of
    -- Attempt to inline native definitions and binary operators
    LuaRef (LuaName name)
      | Just name' <- getEscaped name (state ^. emitEscape)
      , Just EmittedUpvalue { emitTop = VarInline a b } <- state ^. (emitGraph . at name')
      , length e' == a -> case b e' of
          (Seq.Empty, es) -> pure $ EmittedExpr es yield
          (ss, es) -> do
            (ss', vals) <- genYield pushScope' yield es
            pure $ EmittedStmt (ss <> ss') vals
    _ -> pure $ EmittedExpr [LuaCallE (LuaCall f' e')] yield

emitExpr var yield (AnnLam _ TypeArgument{} b) = emitExpr var yield b

emitExpr var yield t@(AnnLam fv (TermArgument v ty) e) = do
  scope <- ask
  graph <- liftedGraph fv
  escape <- use emitEscape

  let (vs, escape') = flip runState escape $ genVars (state . pushVar) v ty (Just e)
      graph' = VarMap.insert (toVar v) (EmittedUpvalue VarUpvalue (simpleVars vs) [v]) graph

      term :: [LuaExpr] = pure . LuaFunction vs . toList . fst3 $ emitTerm scope escape' graph' YieldReturn e
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

emitExpr var yield (AnnExtend fv tbl fs) = do
  {-
    Record extensions have to be emitted as a statement. We build up a
    statement list first, and then.
  -}

  graph <- use emitGraph
  let fs' = sortAtoms graph thd3 fs
  (deps, node) <- runNES fv $ case yield of
    YieldDiscard -> do
      exs' <- traverse (emitAtom . thd3) fs'
      tbl' <- emitAtom tbl
      pure $ EmittedStmt (foldMap (foldMap asStmt) (tbl':exs')) []

    YieldStore vs -> do
      let vs' = head vs
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaAssign vs [LuaTable []] <| emitCopy vs' tbl' <> exs') (simpleVars vs)

    YieldReturn -> do
      let vs' = LuaName (T.pack "__n")
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaLocal [vs'] [LuaTable []] <| emitCopy vs' tbl' <> exs') []

    YieldDeclare var' _ -> do
      vs' <- LuaName <$> pushScope' var'
      exs' <- foldrM (emitRow vs') mempty fs'
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaLocal [vs'] [LuaTable []] <| emitCopy vs' tbl' <> exs') [simpleVar vs']

  pushGraph var (node [var] deps)

  where
    k = T.pack "k"
    v = T.pack "v"
    pairs = LuaName (T.pack "pairs")

    emitRow var (f, _, e) es = (<|es) . LuaAssign [LuaIndex (LuaRef var) (LuaString f)] <$> emitAtom e

    emitCopy var (LuaRef v@LuaName{}) = pure (copy var v)
    emitCopy var tbl =
      let old = LuaName (T.pack "__o")
      in LuaLocal [old] [tbl] <| copy var old <| mempty

    copy var tbl =
      LuaFor [LuaName k, LuaName v] [LuaCallE (LuaCall (LuaRef pairs) [LuaRef tbl])]
         [LuaAssign [LuaIndex (LuaRef var) (LuaRef (LuaName k))] [LuaRef (LuaName v)]]

runNES :: ( MonadReader (EmitScope a) m
          , MonadState (EmitState a) m )
       => VarSet.Set -> StateT (NodeEmitState a) m b
       -> m (VarSet.Set, b)
runNES deps m = do
  s <- get
  (a, NES s' deps') <- runStateT m (NES s deps)
  put s'
  pure (deps', a)

withinExpr :: ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState a) m )
         => a -> EmitYield a -> AnnTerm VarSet.Set a
         -> StateT (NodeEmitState a) m [LuaExpr]
         -> m ()
withinExpr var yield term m = withinTerm var term $ flip EmittedExpr yield <$> m

withinTerm :: ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState a) m )
         => a -> AnnTerm VarSet.Set a
         -> StateT (NodeEmitState a) m ([a] -> VarSet.Set -> EmittedNode a)
         -> m ()
withinTerm var term m = do
  ari <- view emitArity
  prev <- use emitPrev

  let p = isPure ari term
      deps = extractAnn term <> (if p then mempty else prev)
  (deps', result) <- runNES deps m

  -- Update the pure set if needed
  unless p (emitPrev .= VarSet.singleton (toVar var))

  pushGraph var (result [var] deps')

emitAtomS :: ( Occurs a
             , MonadState (NodeEmitState a) m)
          => Atom a
          -> m LuaExpr
emitAtomS a = (\[x] -> x) <$> emitAtom a

emitAtom :: forall a m.
            ( Occurs a
            , MonadState (NodeEmitState a) m)
         => Atom a
         -> m [LuaExpr]
emitAtom (Lit l) = pure [emitLit l]
emitAtom (Ref (toVar -> v) _) = do
  existing <- uses (nodeState . emitGraph) (VarMap.lookup v)
  case existing of
    -- Statements are easy: the dependency is already in the graph so we
    -- don't need to do any checks.
    Nothing -> pure . LuaRef . LuaName <$> uses (nodeState . emitEscape) (getVar v)
    Just EmittedStmt { emitVals = vs } -> pure (map unsimple vs)
    Just EmittedUpvalue { emitVals = vs } -> pure (map unsimple vs)

    Just (EmittedExpr expr (YieldDeclare var ty) binds deps) | usedWhen var == Once -> do
      let var' = toVar var
      testEmitGraph' <- uses (nodeState . emitGraph) (VarMap.delete var')
      deps' <- uses nodeDeps (VarSet.delete var' . VarSet.union deps)
      case hasLoop (VarSet.singleton var') deps' testEmitGraph' of
        Nothing -> do
          (stmts, vals) <- genDeclare pushScope' var ty expr
          let existing' = EmittedStmt { emitStmts  = stmts
                                      , emitVals   = vals
                                      , _emitBinds = binds
                                      , _emitDeps  = deps
                                      }
          nodeState . emitGraph %= VarMap.insert var' existing'
          pure (map unsimple vals)
        Just{} -> do
          nodeDeps .= deps'
          nodeState . emitGraph .= testEmitGraph'
          pure expr
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, vals) <- genYield pushScope' yield expr
      (nodeState . emitGraph) %= VarMap.insert v (EmittedStmt stmts vals binds deps)
      pure (map unsimple vals)
    where
      -- | Detect if the @emitted@ has a loop
      --
      -- TODO: Convert this to a breadth first search - loops are
      -- /probably/ going to be close to the start so there's no point
      -- looping up to the top of the expression tree.
      hasLoop :: VarSet.Set -- ^ The set of nodes on the visiting stack
              -> VarSet.Set -- ^ The set of nodes we're about to visit
              -> EmittedGraph a -- ^ A map of nodes and their edges which have not been visited
              -> Maybe (EmittedGraph a) -- ^ The remaining nodes/edges which have not been visited
      hasLoop visiting toVisit emitted = VarSet.foldr (\v emitted ->
        -- Effectively foldrM, but we don't have an instance for it.
        case emitted of
          Nothing -> Nothing
          Just x -> hasLoop' visiting v x) (Just emitted) toVisit

      hasLoop' :: VarSet.Set -> CoVar -> EmittedGraph a
              -> Maybe (EmittedGraph a)
      hasLoop' visiting v remaining =
        if VarSet.member v visiting then Nothing
        else case VarMap.lookup v remaining of
               Nothing -> Just remaining
               Just e -> hasLoop (VarSet.insert v visiting)
                                 (e ^. emitDeps)
                                 (foldr (VarMap.delete . toVar) remaining (e ^. emitBinds))

-- | A variant of 'emitAtom' which always binds expressions.
--
-- This is suitable for when something is used many times, even when not
-- annotated as such (hence the name).
emitAtomMany :: forall a m.
                ( IsVar a
                , MonadState (EmitState a) m)
             => Atom a
             -> m [LuaExpr]
emitAtomMany (Lit l) = pure [emitLit l]
emitAtomMany (Ref v _) = map unsimple <$> emitVarBinds (toVar v)

-- | Emit the appropriate bindings for a variable.
--
-- This unconditionally promotes an expression to a statement, returning
-- which variables it was bound to.
emitVarBinds :: forall a m.
                ( IsVar a
                , MonadState (EmitState a) m)
             => CoVar -> m [LuaSimple]
emitVarBinds v = do
  existing <- uses emitGraph (VarMap.lookup v)
  case existing of
    -- Statements and upvalues are easy: the dependency is already in the
    -- graph so we don't need to do any checks.
    Nothing -> pure . simpleVar . LuaName <$> uses emitEscape (getVar v)
    Just EmittedStmt { emitVals = vs } -> pure vs
    Just EmittedUpvalue { emitVals = vs } -> pure vs
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, vs) <- genYield pushScope yield expr
      pushGraph v EmittedStmt { emitStmts  = stmts
                              , emitVals   = vs
                              , _emitBinds = binds
                              , _emitDeps  = deps }
      pure vs

-- | Generate the appropriate code for the provided yield.
genYield :: Monad m
          => (a -> m T.Text) -> EmitYield a -> [LuaExpr]
          -> m (Seq LuaStmt, [LuaSimple])
genYield _ YieldReturn es = pure (pure (LuaReturn es), [])
genYield _ YieldDiscard es = pure (foldMap asStmt es, [])
genYield _ (YieldStore vs) es = pure (pure (LuaAssign vs es), simpleVars vs)
genYield e (YieldDeclare v ty) es = genDeclare e v ty es

-- | Emit a declaration for a variable and a collection of expressions
--
-- This returns a 'LuaLocal' statement binding such expressions and the
-- variables which were bound. We do not emit bindings for expressions
-- which are just variables, returning variable directly.
genDeclare :: Monad m
            => (a -> m T.Text) -> a -> Type a -> [LuaExpr]
            -> m (Seq LuaStmt, [LuaSimple])
genDeclare escape v ty es
  -- If all expressions are simple, then just return them
  | Just vals <- traverse simpleOf es = pure (mempty, vals)
  -- Unpack tuples into multiple assignments
  | ValuesTy ts <- ty = do
      v' <- escape v
      let (rs, lhs, rhs) = tupleVars v' 1 ts es
      pure (pure (LuaLocal lhs rhs), rs)
  -- Generate @local function@ if needed, as it's a wee bit "nicer".
  | [LuaFunction a b] <- es = do
      v' <- escape v
      let var = LuaName v'
      pure (pure (LuaLocalFun var a b), [simpleVar var])
  -- Just do a normal binding
  | otherwise = do
      v' <- escape v
      let var = LuaName v'
      pure (pure (LuaLocal [var] es), [simpleVar var])

  where
    -- | Generate a set of bindings for a tuple variable
    tupleVars :: T.Text -> Int -> [Type a] -> [LuaExpr]
            -> ([LuaSimple], [LuaVar], [LuaExpr])
    tupleVars _ _ [] es = ([], [], es)
    tupleVars v n (_:ts) [] =
      let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
          var = LuaName (v <> T.pack ('_':show n))
      in (simpleVar var : rs, var : lhs, rhs)
    tupleVars v n (_:ts) (e:es)
      | Just e' <- simpleOf e =
        let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
        in (e' : rs, lhs, rhs)
      | otherwise =
        let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
            var = LuaName (v <> T.pack ('_':show n))
        in (simpleVar var : rs, var : lhs, e : rhs)

-- | Generate the variables needed for this binding
--
-- This is effectively a simplified version of 'genDeclare' when you do
-- not know the RHS.
genVars :: (IsVar a, Monad m)
        => (a -> m T.Text) -- ^ The fresh variable generator function
        -> a -> Type a -> Maybe (AnnTerm b a)
        -- ^ The variable, its type and an optional body to extract the names from.
        -> m [LuaVar]
genVars es v ValuesTy{}
  (Just (AnnMatch _ (Ref v' _) (Arm { _armPtrn = PatValues cs }:_)))
  | toVar v == toVar v' = traverse extract cs where
      extract (Capture n _) = LuaName <$> es n
genVars es v (ValuesTy vs) _ = do
  v' <- es v
  let go _ [] = []
      go n (_:ts) = LuaName (v' <> T.pack ('_':show n)) : go (n + 1) ts
  pure (go (1 :: Int) vs)
genVars es v _ _ = pure . LuaName <$> es v

-- | Convert a literal into a Lua expression
emitLit :: Literal -> LuaExpr
emitLit (Int x)   = LuaInteger (fromIntegral x)
emitLit (Float x) = LuaNumber x
emitLit (Str str) = LuaString str
emitLit LitTrue   = LuaTrue
emitLit LitFalse  = LuaFalse
emitLit Unit      = LuaRef (LuaName "__builtin_unit") -- Evil, but it works!
emitLit RecNil    = LuaTable []

emitStmt :: forall a m. (Occurs a, MonadState TopEmitState m)
         => [AnnStmt VarSet.Set a] -> m (Seq LuaStmt)
emitStmt [] = pure mempty
emitStmt (Foreign n t s:xs) = do
  n' <- pushTopScope n
  topArity %= flip extendForeign (n, t)

  let ex = case parseExpr (SourcePos "_" 0 0) (s ^. lazy) of
        Right x -> x
        Left _ -> LuaBitE s

      normalDef :: m (Seq LuaStmt)
      normalDef = do
        topVars %= VarMap.insert (toVar n) (VarUpvalue, simpleVars [LuaName n'])
        pure . pure $ LuaLocal [LuaName n'] [ex]

      basicArity (ForallTy Irrelevant (ValuesTy ts) _) = length ts
      basicArity (ForallTy Irrelevant _ _) = 1
      basicArity (ForallTy Relevant{} _ r) = basicArity r
      basicArity _ = -1

  def <- case ex of
    LuaRef{} -> normalDef

    LuaFunction as bod
      | length as == basicArity t
      , shouldInline as bod -> do
          -- We've got an expression which can be inlined, push a 'VarInline'!
          let inline = case last bod of
                LuaReturn rs -> \xs ->
                  let s' = Map.fromList $ zip as xs
                  in (substStmt s' <$> Seq.fromList (init bod), map (substExpr s') rs)
                _ -> \xs ->
                  let s' = Map.fromList $ zip as xs
                  in (substStmt s' <$> Seq.fromList bod, [])

          topVars %= VarMap.insert (toVar n)
            ( VarInline (length as) inline
            , simpleVars [LuaName n'])
          -- This one may never be visited normally, so we'll push the
          -- extra-variable version of it instead.
          topExVars %= VarMap.insert (toVar n) ([], pure $ LuaLocal [LuaName n'] [ex])
          pure mempty

    _ | Just ex' <- simpleOf ex -> do
          -- We're a simple expression, so we're guaranteed to never see this!
          topVars %= VarMap.insert (toVar n) (VarUpvalue, [ex'])
          pure mempty

    _ -> normalDef
  (def<>) <$> emitStmt xs

emitStmt (Type _ cs:xs) = do
  traverse_ emitConstructor cs
  topArity %= flip extendPureCtors cs
  emitStmt xs

  where
    emitConstructor (var, ty) = do
      var' <- pushTopScope var
      let tag = LuaString var'
          name = LuaName var'
          ctor = if arity ty == 0
                 then [luaStmts|local $name = { __tag = %tag }|]
                 else [luaStmts|local function $name(x) return { __tag = %tag, x } end|]
      topVars %= VarMap.insert (toVar var) (VarUpvalue, [simpleVar (LuaName var')])
      topExVars %= VarMap.insert (toVar var) ([], Seq.fromList ctor)

emitStmt (StmtLet (One (v, ty, e)):xs) = do
  TopEmitState { _topArity = ari, _topEscape = esc, _topVars = vars } <- get
  let yield = if usedWhen v == Dead then YieldDiscard else YieldDeclare v ty
      (stmts, binds, esc') =
        emitTerm (EmitScope ari) esc
                 (VarMap.mapWithKey (\v (d, b) -> EmittedUpvalue d b [fromVar v]) vars)
                 yield e

  topArity  %= flip extendPureLets [(v, ty, e)]
  topVars   %= VarMap.insert (toVar v) (VarUpvalue, binds)
  topEscape .= esc'
  (stmts<>) <$> emitStmt xs

emitStmt (StmtLet (Many vs):xs) = do
  binds <- traverse (\(v, ty, _) -> genVars pushTopScope v ty Nothing) vs
  topArity %= flip extendPureLets vs
  topVars  %= \x -> foldr (\(v, b) -> VarMap.insert (toVar . fst3 $ v) (VarUpvalue, simpleVars b)) x (zip vs binds)

  TopEmitState { _topArity = ari, _topEscape = esc, _topVars = vars } <- get

  let graph :: EmittedGraph a = VarMap.mapWithKey (\v (d, b) -> EmittedUpvalue d b [fromVar v]) vars
      (stmt, esc') = foldl' (\(s, esc') ((_, _, e), v') ->
        let (s', _, esc'') = emitTerm (EmitScope ari) esc' graph (YieldStore v') e
        in (s <> s', esc'')) (mempty, esc) (zip vs binds)

  -- We don't strictly need to share escape scopes across expressions, but it helps us avoid shadowing
  topEscape .= esc'

  xs' <- emitStmt xs
  pure $ case stmt of
    (LuaAssign [v] [LuaFunction args bod] :<| Empty) -> LuaLocalFun v args bod <| xs'
    _ -> LuaLocal (mconcat binds) [] <| stmt <> xs'

-- | Push a new node into the emitting graph
pushGraph :: (IsVar b, MonadState (EmitState a) m) => b -> EmittedNode a -> m ()
pushGraph var node = emitGraph %= VarMap.insert (toVar var) node

-- | Push a variable into the current scope
pushScope :: (IsVar a, MonadState (EmitState a) m) => a -> m T.Text
pushScope v = emitEscape %%= pushVar v

-- | Push a variable into the current scope
pushTopScope :: (IsVar a, MonadState TopEmitState m) => a -> m T.Text
pushTopScope v = topEscape %%= pushVar v

-- | Push a variable into the current scope
pushScope' :: (IsVar a, MonadState (NodeEmitState a) m) => a -> m T.Text
pushScope' v = nodeState . emitEscape %%= pushVar v

-- | Convert an expression into a set of Lua statements
asStmt :: LuaExpr -> Seq LuaStmt
asStmt (LuaTable fs) = foldr ((<>) . asStmt . snd) mempty fs
asStmt (LuaBinOp a _ b) = asStmt a <> asStmt b
asStmt (LuaCallE x) = pure (LuaCallS x)
asStmt _ = mempty

-- | Convert an expression and a yield context to a set of Lua statements.
--
-- Note, this assumes that the result of the expression is not directly
-- used, and so 'YieldDeclare's will be treated like 'YieldDiscard'.
yieldStmt :: EmitYield a -> [LuaExpr] -> Seq LuaStmt
yieldStmt YieldReturn es = pure (LuaReturn es)
yieldStmt (YieldStore vs) es = pure (LuaAssign vs es)
yieldStmt YieldDiscard es = foldMap asStmt es
yieldStmt (YieldDeclare _ _) es = foldMap asStmt es

-- | Get a simple variable from a normal one
simpleOf :: LuaExpr -> Maybe LuaSimple
simpleOf x | check x = Just (LuaSimp x) where
  -- Basic keywords are obviously simple
  check LuaNil   = True
  check LuaTrue  = True
  check LuaFalse = True
  -- We consider literals simple for the sake of argument
  check LuaInteger{} = True
  check (LuaString x) | T.length x <= 16 = True
  -- For the sake of argument, references to variables are simple
  check (LuaRef LuaName{}) = True
  check _ = False
simpleOf _ = Nothing

simpleVar :: LuaVar -> LuaSimple
simpleVar = LuaSimp . LuaRef

simpleVars :: [LuaVar] -> [LuaSimple]
simpleVars = map simpleVar

-- | Sort a series of atoms in such a way that we can minimise the number
-- of temporary variables required to emit this.
--
-- This code is a little sub-optimal, as it will lose the original
-- ordering, even when it doesn't need to. Ideally we could build a
-- partial ordering of the "interesting" terms, and then merge that with
-- the original list.
sortAtoms :: forall a b. Occurs a => EmittedGraph a -> (b -> Atom a) -> [b] -> [b]
sortAtoms graph ex atoms =
  let (lits, vars) = spanVars atoms
  in if VarMap.size vars <= 1
     then atoms
     else sortVars vars ++ lits
  where
    -- | Split atoms into variables nodes and "boring" literals.
    spanVars :: [b] -> ([b], VarMap.Map b)
    spanVars [] = mempty
    spanVars (x:xs) =
      (case ex x of
          Ref v _
            | Just (EmittedExpr _ (YieldDeclare var _) _ deps) <- VarMap.lookup (toVar v) graph
            , usedWhen var == Once
            , not (VarSet.null deps)
            -> second (VarMap.insert (toVar v) x)
          _ -> first (x:)) (spanVars xs)

    sortVars :: VarMap.Map b -> [b]
    sortVars vs =
      let (_, pend, xs) = VarMap.foldrWithKey (\k _ -> sortVisit k) (mempty, vs, []) vs
      in if VarMap.null pend then reverse xs
         else error "Pending map should be empty"

    sortVisit :: CoVar -- ^ The variable to visit
              -> (VarSet.Set, VarMap.Map b, [b]) -- ^ The visited set, the pending nodes, and the current queue
              -> (VarSet.Set, VarMap.Map b, [b]) -- ^ Resulting list of pending nodes and the current queue
    sortVisit var skip@(visited, pend, xs)
      | VarMap.null pend = skip
      | VarSet.member var visited = skip
      | otherwise = case VarMap.lookup var graph of
          Nothing -> skip
          Just node ->
            let visited' = foldr (VarSet.insert . toVar) visited (node ^. emitBinds)
                pend' = foldr (VarMap.delete . toVar) pend (node ^. emitBinds)

                (visited'', pend'', xs') = VarSet.foldr sortVisit (visited', pend', xs) (node ^. emitDeps)
            in ( visited'', pend''
               , foldr (maybe id (:) . flip VarMap.lookup pend . toVar) xs' (node ^. emitBinds) )

patternBindings :: Occurs a => Pattern a -> [LuaExpr] -> [(a, [LuaExpr])]
patternBindings (PatLit _) _     = []
patternBindings PatWildcard _    = []
patternBindings (Constr _) _     = []
patternBindings (Destr _ p) [vr] = captureBinding p [LuaRef (LuaIndex vr (LuaInteger 1))]
patternBindings (PatExtend p rs) [vr] = captureBinding p [vr] ++ concatMap (index vr) rs where
  index vr (var', pat) = captureBinding pat [LuaRef (LuaIndex vr (LuaString var'))]
patternBindings (PatValues ps) vr = mconcat (zipWith (\p v -> captureBinding p [v]) ps vr)
patternBindings _ _ = error "Mismatch between pattern and expression arity"

captureBinding :: Occurs a => Capture a -> [LuaExpr] -> [(a, [LuaExpr])]
captureBinding (Capture n _) v
  | doesItOccur n = [(n, v)]
  | otherwise = []

-- | Generate a new graph for the provided set of patterns
patternGraph :: ( Occurs a
                , MonadReader (EmitScope a) m
                , MonadState (EmitState a) m )
             => Atom a -> [LuaExpr] -> AnnArm VarSet.Set a
             -> EmittedGraph a
             -> m (EmittedGraph a)
patternGraph test test' Arm { _armPtrn = p, _armVars = vs } graph = do
  let deps = freeInAtom test
      (once, many) = partition ((==Once) . usedWhen . fst) (patternBindings p test')

  graph' <- case many of
    [] -> pure graph
    [(v, expr)] -> do
      (body, vars) <- genDeclare pushScope v (getTy v) expr
      pure (VarMap.insert (toVar v) (EmittedStmt body vars [v] deps) graph)

    many@((vFst, _):ps) -> do
      -- If we've got multiple patterns which will be bound, we can merge them into one binding, with all other nodes
      -- just pointing to the first one.
      stmts <- traverse (\(v, expr) -> genDeclare pushScope v (getTy v) expr) many
      let (_, vsFst) = head stmts

      pure $ foldr
        (\((v, _), (_, vs)) -> VarMap.insert (toVar v) (EmittedStmt mempty vs [v] (VarSet.singleton (toVar vFst))))
        (VarMap.insert (toVar vFst) (EmittedStmt (foldr (mergeLocs . fst) mempty stmts) vsFst [vFst] deps) graph)
        (zip ps (tail stmts))

  -- Boring expressions
  pure $ foldr
    (\(v, expr) -> VarMap.insert (toVar v) (EmittedExpr expr (YieldDeclare v (getTy v)) [v] deps))
    graph' once

  where
    getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

    -- | Merge two adjacent local definitions
    mergeLocs (LuaLocal vs es :<| Empty) (LuaLocal vs' es' :<| rest)
      | length vs == length es = LuaLocal (vs ++ vs') (es ++ es') <| rest
    mergeLocs l r = l <> r

patternTest :: forall a. IsVar a => EscapeScope -> Pattern a -> [LuaExpr] ->  LuaExpr
patternTest _ PatWildcard _      = LuaTrue
patternTest _ (PatLit RecNil) _  = LuaTrue
patternTest _ PatExtend{} _      = LuaTrue
patternTest _ PatValues{} _      = LuaTrue
patternTest _ (PatLit l)  [vr]   = LuaBinOp vr "==" (emitLit l)
patternTest s (Constr con) [vr]  = tag s con vr
patternTest s (Destr con _) [vr] = tag s con vr
patternTest _ _ _ = undefined

tag :: IsVar a => EscapeScope -> a -> LuaExpr -> LuaExpr
tag scp con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaString "__tag"))) "==" (LuaString (getVar con scp))
