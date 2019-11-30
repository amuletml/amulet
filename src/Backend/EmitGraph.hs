{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , StandaloneDeriving
  , TemplateHaskell
  , TypeFamilyDependencies
  , UndecidableInstances
  , ViewPatterns #-}
module Backend.EmitGraph
 ( Emitter(..)
 , EmittedNode(..), emitBinds, emitDeps
 , EmittedGraph
 , EmitState(..), emitGraph, emitPrev, emitEscape
 , NodeEmitState(..), nodeState, nodeDeps, nodeMerged
 , EmitScope(..), emitArity

 -- * Scope and graph modification functions
 , pushGraph
 , pushScope
 , pushScope'

 -- * Emitting basic terms
 , emitRef
 , emitVarBinds

 -- * Node emit state
 , runNES
 , withinExpr
 , withinTerm

 -- * Working with graphs
 , vReturn
 , liftedGraph
 , flushDeps
 , flushGraph
 , nodeStmts
 , sortAtoms
 ) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Bifunctor

import qualified Core.Builtin as B
import Core.Occurrence
import Core.Arity
import Core.Core
import Core.Var

import Backend.Escape

import Text.Pretty.Semantic
import Text.Dot

class Emitter a where
  -- | The contents of an upvalue. Generally this will be empty, but you
  -- may wish to include additional information, such as for
  -- native/foreign calls.
  type EUpvalue a = r | r -> a

  -- | An atom in the target language. This should be a name or constant
  -- - anything which can be emitted multiple times.
  type EAtom a = r | r -> a

  type EExpr a = r | r -> a -- ^ An expression in the target language.
  type EStmt a -- ^ A statement in the target language.

  -- | Keeps track of how the current term will be used.
  type EYield a = r | r -> a

  -- | The basic upvalue, with no additional information
  upvalue :: EUpvalue a

  -- | Emit a declaration for a variable and a collection of expressions
  --
  -- This returns a series of bindings for the given expressions and the
  -- variables which were bound. We do not emit bindings for expressions
  -- which are just variables, returning variable directly.
  genDeclare :: Monad m
             => (v -> m T.Text) -> v -> Type -> [EExpr a]
             -> m (EStmt a, [EAtom a])

  -- | Convert a simple atom into an expression.
  atomToExpr :: EAtom a -> EExpr a

  -- | Construct an expr from a variable name
  mkExprRef :: T.Text -> EExpr a

  -- | Construct an atom from a variable name
  mkAtomRef :: T.Text -> EAtom a

  -- | Generate the appropriate code for the provided yield.
  genYield :: Monad m
           => (CoVar -> m T.Text) -> EYield a -> [EExpr a]
           -> m (EStmt a, [EAtom a])

  -- | Acts as a view pattern for determining if this is a yield term
  -- which should declare this variable.
  -- It returns the variable's name, its type, and how it is used.
  yieldDeclare :: EYield a -> Maybe (CoVar, Type, Occurrence)

  -- | Convert an expression and a yield context to a set of statements.
  --
  -- This may assume that the result of the expression is not directly
  -- used, and so declarations can be treated as a discard.
  yieldStmt :: EYield a -> [EExpr a] -> EStmt a

-- | A node in the  graph of emitted expressions and statements
--
-- 'EmittedExpr's should be merged inline into another expression if
-- doing so does not introduce loops into the graph. Meanwhile one should
-- consume the 'EmittedStmt' 'emitBound' variables from the statement
-- instead.
data EmittedNode a
  = EmittedExpr
    { emitExprs  :: [EExpr a]  -- ^ The expression(s) which will be emitted
    , emitYield  :: EYield a   -- ^ The result we should be binding this to if consumed
    , _emitBinds :: VarSet.Set -- ^ The variables bound in the statement. Used when traversing the dependency
                               -- graph.
    , _emitDeps  :: VarSet.Set -- ^ The dependencies for this node. Consuming it should assimilate them.
    }
  | EmittedStmt
    { emitStmts  :: EStmt a     -- ^ The statements required before this can be evaluated
    , emitVals   :: [EAtom a]  -- ^ Each value or variable declared by this node.
    , _emitBinds :: VarSet.Set -- ^ The variables bound in the statement. Used when traversing the dependency
                               -- graph.
    , _emitDeps  :: VarSet.Set -- ^ The dependencies for this node. Consuming need not assimilate them.
    }
  | EmittedUpvalue
    { emitTop    :: EUpvalue a -- ^ The top level declaration to consume
    , emitVals   :: [EAtom a]  -- ^ Each value or variable declared by this node.
    , _emitBinds :: VarSet.Set -- ^ A singleton set, containing this variable's upvalue.
    }

deriving instance (Show (EUpvalue a), Show (EAtom a), Show (EExpr a), Show (EStmt a), Show (EYield a)) => Show (EmittedNode a)

-- | The graph of all 'EmittedNode's.
type EmittedGraph a = VarMap.Map (EmittedNode a)

-- | The current state for the expression/term emitter. This is thread
-- through a 'MonadState' instance.
data EmitState a = EmitState
  { _emitGraph  :: EmittedGraph a
  -- | The set of variables this term should depend on. These will be the
  -- variables bound by the previous non-pure term.
  , _emitPrev   :: VarSet.Set
  , _emitEscape :: EscapeScope
  }

-- | The current scope for the expression/term emitter. This is thread
-- through a 'MonadReader' instance.
newtype EmitScope a = EmitScope
  { _emitArity :: ArityScope
  }
  deriving Show

deriving instance (Show (EUpvalue a), Show (EAtom a), Show (EExpr a), Show (EStmt a), Show (EYield a)) => Show (EmitState a)

-- | The state for emitting a single node.
--
-- This is a wrapper for 'EmitState', but also tracking dependencies for
-- this node. One generally uses 'runNES' in order to evaluate this.
data NodeEmitState a = NES
  { _nodeState  :: EmitState a
  , _nodeDeps   :: VarSet.Set
  , _nodeMerged :: VarSet.Set
  }

deriving instance (Show (EUpvalue a), Show (EAtom a), Show (EExpr a), Show (EStmt a), Show (EYield a)) => Show (NodeEmitState a)

makeLenses ''EmittedNode
makeLenses ''EmitState
makeLenses ''NodeEmitState
makeLenses ''EmitScope

instance (Pretty (EExpr a), Pretty (EStmt a)) => Pretty (VarMap.Map (EmittedNode a)) where
  pretty = drawGraph disp . toGraph where
    disp v@(CoVar id _ _) = text (covarDisplayName v) <> "_" <> int' id
    int' x | x < 0 = "_" <> int (-x)
           | otherwise = int x

-- | Push a new node into the emitting graph
pushGraph :: MonadState (EmitState a) m => EmittedNode a -> m ()
pushGraph node = emitGraph %= \g -> VarSet.foldr (flip VarMap.insert node . toVar) g (node ^. emitBinds)

-- | Push a variable into the current scope
pushScope' :: (IsVar v, MonadState (NodeEmitState a) m) => v -> m T.Text
pushScope' v = nodeState . emitEscape %%= pushVar v

-- | Push a variable into the current scope
pushScope :: (IsVar v, MonadState (EmitState a) m) => v -> m T.Text
pushScope v = emitEscape %%= pushVar v

-- | Emit a reference to a variable, either as an inline expression or as
-- a variable reference.
emitRef :: forall e m.
            (Emitter e, MonadState (NodeEmitState e) m)
         => CoVar
         -> m [EExpr e]
emitRef v = do
  existing <- uses (nodeState . emitGraph) (VarMap.lookup v)
  case existing of
    -- Statements are easy: the dependency is already in the graph so we
    -- don't need to do any checks.
    Nothing -> pure . mkExprRef <$> uses (nodeState . emitEscape) (getVar v)
    Just EmittedStmt { emitVals = vs } -> pure (map atomToExpr vs)
    Just EmittedUpvalue { emitVals = vs } -> pure (map atomToExpr vs)

    Just (EmittedExpr expr (yieldDeclare -> Just (var, ty, Once)) binds deps) -> do
      when (VarSet.notMember (toVar var) binds) (pure $ error "Variable mismatch")

      let var' = toVar var
      testEmitGraph' <- uses (nodeState . emitGraph) (\s -> VarSet.foldr VarMap.delete s binds)
      deps' <- uses nodeDeps (flip VarSet.difference binds . VarSet.union deps)
      case hasLoop (VarSet.singleton var') deps' testEmitGraph' of
        Nothing -> do
          (stmts, vals) <- genDeclare pushScope' var ty expr
          let node = EmittedStmt { emitStmts  = stmts
                                 , emitVals   = vals
                                 , _emitBinds = binds
                                 , _emitDeps  = deps
                                 }
          nodeState . emitGraph %= \g -> VarSet.foldr (`VarMap.insert` node) g binds
          pure (map atomToExpr vals)
        Just{} -> do
          nodeDeps .= deps'
          nodeMerged %= VarSet.union binds
          nodeState . emitGraph .= testEmitGraph'
          pure expr
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, vals) <- genYield pushScope' yield expr
      let node = EmittedStmt stmts vals binds deps
      (nodeState . emitGraph) %= \g -> VarSet.foldr (`VarMap.insert` node) g binds
      pure (map atomToExpr vals)
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
               -- If we're not in the graph, skip.
               Nothing -> Just remaining
               -- If we're a node which has been embedded into the top node then skip.
               -- I'm not 100% sure this is correct.
               Just e -> hasLoop (VarSet.insert v visiting)
                                 (e ^. emitDeps)
                                 (VarSet.foldr VarMap.delete remaining (e ^. emitBinds))

-- | Emit the appropriate bindings for a variable.
--
-- This unconditionally promotes an expression to a statement, returning
-- which variables it was bound to.
emitVarBinds :: forall m a.
                (Emitter a, MonadState (EmitState a) m)
             => CoVar -> m [EAtom a]
emitVarBinds v = do
  existing <- uses emitGraph (VarMap.lookup v)
  case existing of
    -- Statements and upvalues are easy: the dependency is already in the
    -- graph so we don't need to do any checks.
    Nothing -> pure . mkAtomRef <$> uses emitEscape (getVar v)
    Just EmittedStmt { emitVals = vs } -> pure vs
    Just EmittedUpvalue { emitVals = vs } -> pure vs
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, vs) <- genYield pushScope yield expr
      pushGraph EmittedStmt { emitStmts  = stmts
                            , emitVals   = vs
                            , _emitBinds = binds
                            , _emitDeps  = deps }
      pure vs


-- | Run a 'NodeEmitState' monad
runNES :: MonadState (EmitState a) m
       => VarSet.Set -> StateT (NodeEmitState a) m b
       -> m (VarSet.Set, VarSet.Set, b)
runNES deps m = do
  s <- get
  (a, NES s' deps' binds) <- runStateT m (NES s deps mempty)
  put s'
  pure (deps', binds, a)

-- | Run a function within an expression
withinExpr :: ( Occurs v
              , MonadReader (EmitScope v) m
              , MonadState (EmitState a) m )
           => v -> EYield a -> AnnTerm VarSet.Set v
           -> StateT (NodeEmitState a) m [EExpr a]
           -> m ()
withinExpr var yield term m = withinTerm var term $ flip EmittedExpr yield <$> m

withinTerm :: ( Occurs v
              , MonadReader (EmitScope v) m
              , MonadState (EmitState a) m )
           => v -> AnnTerm VarSet.Set v
           -> StateT (NodeEmitState a) m (VarSet.Set -> VarSet.Set -> EmittedNode a)
           -> m ()
withinTerm var term m = do
  ari <- view emitArity
  prev <- use emitPrev

  -- We add the previous term as a dependency if we're impure (obviously), or if
  -- we're the last term in a block. Yes, this is an ugly hack - ideally it'd be
  -- done in emitTerm.
  let p = isPure ari term
      deps = extractAnn term <> (if not p || toVar var == vReturn then prev else mempty)
  (deps', binds, result) <- runNES deps m

  -- Update the pure set if needed
  unless p (emitPrev .= one var)

  pushGraph (result (VarSet.insert (toVar var) binds) deps')

-- | A magic variable used to represent the return value
vReturn :: CoVar
vReturn = B.backendRet

-- | Lift all nodes in a graph, converting them to upvalues.
liftedGraph :: forall a m.
               (Emitter a, MonadState (EmitState a) m)
            => VarSet.Set -> m (EmittedGraph a)
liftedGraph = VarSet.foldr liftNode (pure mempty) where
  liftNode :: CoVar -> m (EmittedGraph a) -> m (EmittedGraph a)
  liftNode v m = do
    existing <- uses emitGraph (VarMap.lookup v)
    case existing of
      Just n@EmittedUpvalue{} -> VarMap.insert v n <$> m
      _ -> do
        n <- emitVarBinds v
        VarMap.insert v (EmittedUpvalue upvalue n (VarSet.singleton v)) <$> m

-- | Worker function to remove a node from a graph and emit all its
-- dependencies, returning the sequence of statements and the modified
-- graph.
--
-- This is used in 'emitTerm' and 'emitTermES'.
flushGraph :: (Emitter a, Monoid (EStmt a))
           => CoVar -> VarSet.Set -> EmittedGraph a
           -> (EStmt a, EmittedGraph a)
flushGraph var extra g =
  case VarMap.lookup var g of
    Nothing -> (mempty, g)
    Just node -> first (<>nodeStmts node) (flushDeps node extra g)

-- | Worker function to remove all of a node's dependencies and emit
-- them, returning the statements and the modified graph.
--
-- This is used by 'emitTerm' and 'emitTermES'
flushDeps :: (Emitter a, Monoid (EStmt a))
          => EmittedNode a -> VarSet.Set -> EmittedGraph a
          -> (EStmt a, EmittedGraph a)
flushDeps node extra g =
  let
    -- Remove this from the graph
    g' = VarSet.foldr VarMap.delete g (node ^. emitBinds)
  in VarSet.foldr
     (\v (s, g) -> first (s<>) (flushGraph v mempty g))
     (mempty, g') (node ^. emitDeps <> extra)

-- | Extract the statements from this node.
nodeStmts :: (Emitter a, Monoid (EStmt a)) => EmittedNode a -> EStmt a
nodeStmts EmittedStmt { emitStmts = s } = s
nodeStmts EmittedExpr { emitExprs = es, emitYield = yield } = yieldStmt yield es
nodeStmts EmittedUpvalue{} = mempty

-- | Sort a series of atoms in such a way that we can minimise the number
-- of temporary variables required to emit this.
--
-- This code is a little sub-optimal, as it will lose the original
-- ordering, even when it doesn't need to. Ideally we could build a
-- partial ordering of the "interesting" terms, and then merge that with
-- the original list.
sortAtoms :: forall a b.
             Emitter a
          => EmittedGraph a
          -> (b -> Atom) -> [b] -> [b]
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
            | Just (EmittedExpr _ (yieldDeclare -> Just (_, _, Once)) _ deps) <- VarMap.lookup (toVar v) graph
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
            let visited' = VarSet.foldr VarSet.insert visited (node ^. emitBinds)
                pend' = VarSet.foldr VarMap.delete pend (node ^. emitBinds)

                (visited'', pend'', xs') = VarSet.foldr sortVisit (visited', pend', xs) (node ^. emitDeps)
            in ( visited'', pend''
               , VarSet.foldr (maybe id (:) . flip VarMap.lookup pend) xs' (node ^. emitBinds) )



toGraph :: (Pretty (EExpr a), Pretty (EStmt a)) => EmittedGraph a -> Graph CoVar
toGraph = Graph DirectedGraph
        . VarMap.foldrWithKey (\v node xs -> gen v node ++ xs) []
  where
    gen _ EmittedUpvalue{} = []
    gen var node
      = Node (defaultInfo { label = Just (display . renderPretty 0.8 100 $ pretty var <> ":=" <> genNode var node) }) var
      : ( if var /= main node then [] else
          VarSet.foldr (\var' xs -> Edge defaultInfo var var' : xs) [] (node ^. emitDeps)
          ++ VarSet.foldr (\var' xs -> Edge defaultInfo { style = Just Dashed } var' var : xs) [] (VarSet.delete var (node ^. emitBinds)) )

    -- | Try to determine the "primary" node of this term. This is actually
    -- entirely arbitrary - the only requirement is that it is equal for all
    -- nodes in this group.
    main = head . VarSet.toList . (^. emitBinds)

    genNode var node@(EmittedExpr expr _ _ _)
      | var == main node = vsep . map pretty $ expr
      | otherwise = "Subsumed with " <+> pretty (main node)
    genNode var node@(EmittedStmt stmt _ _ _)
      | var == main node = pretty stmt
      | otherwise = "Subsumed with" <+> pretty (main node)
    genNode _ EmittedUpvalue{} = "Upvalue"

one :: IsVar a => a -> VarSet.Set
one = VarSet.singleton . toVar
