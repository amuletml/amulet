{-# LANGUAGE OverloadedStrings, NamedFieldPuns, FlexibleContexts, TupleSections, ViewPatterns, ScopedTypeVariables #-}
module Backend.Lua.Emit
  ( emitStmt
  , TopEmitState(..)
  , defaultEmitState
  , ops, remapOp, escapeScope
  ) where

import Control.Monad.Reader
import Control.Monad.State
import Control.Arrow (first)
import Control.Lens

import qualified Data.Sequence as Seq
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import Data.Sequence (Seq((:<|)))
import qualified Data.Text as T
import Data.Traversable
import Data.Foldable
import Data.Triple
import Data.Maybe

import Core.Occurrence
import Core.Builtin
import Core.Arity
import Core.Types
import Core.Core
import Core.Var

import Language.Lua.Parser
import Language.Lua.Syntax

import Backend.Escape

-- | A magic variable used to represent the return value
vReturn :: CoVar
vReturn = CoVar (-100) "<<ret>>" ValueVar

-- | A node in the  graph of emitted expressions and statements
--
-- 'EmittedExpr's should be merged inline into another expression if
-- doing so does not introduce loops into the graph. Meanwhile one should
-- consume the 'EmittedStmt' 'emitBound' variables from the statement
-- instead.
data EmittedNode a
  = EmittedExpr
    { emitExprs :: [LuaExpr]   -- ^ The expression(s) which will be emitted
    , emitYield :: EmitYield a -- ^ The result we should be binding this to if consumed
    , emitBinds :: [a]         -- ^ The variables bound in the statement. Purely used when traversing the dependency
                               -- graph.
    , emitDeps  :: VarSet.Set  -- ^ The dependencies for this node. Consuming it should assimilate them.
    }
  | EmittedStmt
    { emitStmts :: Seq LuaStmt -- ^ The statements required before this can be evaluated
    , emitBound :: [LuaVar]    -- ^ The variable(s) bound.
    , emitBinds :: [a]         -- ^ The variables bound in the statement. Purely used when traversing graphs.
    , emitDeps  :: VarSet.Set  -- ^ The dependencies for this node. Consuming need not assimilate them.
    }
  | EmittedUpvalue
    { emitBound :: [LuaVar]    -- ^ The variable(s) bound.
    , emitBinds :: [a]         -- ^ The variable bound in this statement. Purely used when traversing graphs.
    , emitDeps  :: VarSet.Set  -- ^ An empty set of dependencies for this node
    }
  deriving (Show)

-- | The graph of all 'EmittedNode's.
type EmittedGraph a = VarMap.Map (EmittedNode a)

-- | The top-level emitting state. This is substantially simpler than the
-- more general 'EmitState' as it need node maintain an expression graph.
data TopEmitState = TopEmitState
  { topVars   :: VarMap.Map [LuaVar]
  , topArity  :: ArityScope
  , topEscape :: EscapeScope
  }
  deriving (Show)

-- | The current state for the expression/term emitter. This is thread
-- through a 'MonadState' instance.
data EmitState a = EmitState
  { emitGraph  :: EmittedGraph a
  , emitPrev   :: VarSet.Set
  , emitEscape :: EscapeScope
  }
  deriving (Show)

-- | The current scope for the expression/term emitter. This is thread
-- through a 'MonadReader' instance.
newtype EmitScope a = EmitScope
  { emitArity :: ArityScope
  }
  deriving (Show)

-- | Controls how one of more variables should be returned from a
-- statement
data EmitYield a
  = YieldReturn -- ^ Return the expression(s) using 'LuaReturn'
  | YieldDiscard -- ^ Discard this expression
  | YieldStore [LuaVar] -- ^ Assign this expression(s) to these variables
  | YieldDeclare a (Type a) -- ^ Declare this variable
  deriving (Show)

-- | The state for emitting a single node.
--
-- This is a wrapper for 'EmitState', but also tracking dependencies for
-- this node. One generally uses 'runNES' in order to evaluate this.
data NodeEmitState a = NES
  { nodeState :: EmitState a
  , nodeDeps  :: VarSet.Set
  }

-- | The default (initial) state for the emitter.
defaultEmitState :: TopEmitState
defaultEmitState = TopEmitState mempty emptyScope escapeScope

liftedGraph :: forall a m.
               ( IsVar a
               , MonadReader (EmitScope a) m
               , MonadState (EmitState a) m )
            => VarSet.Set -> m (EmittedGraph a)
liftedGraph = VarSet.foldr liftNode (pure mempty) where
  liftNode :: CoVar -> m (EmittedGraph a) -> m (EmittedGraph a)
  liftNode v m = do
    n <- emitVarBinds v
    VarMap.insert v (EmittedUpvalue n [fromVar v] mempty) <$> m

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
  escape <- gets emitEscape
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
  escape <- gets emitEscape
  pure $ emitTermES scope escape graph yield term

-- | A wrapper for 'emitTermES' which converts the result to a sequence
-- of 'LuaStmt's.
emitTerm :: forall a. Occurs a
         => EmitScope a -> EscapeScope -> EmittedGraph a
         -> EmitYield a -> AnnTerm VarSet.Set a
         -> (Seq LuaStmt, [LuaVar], EscapeScope)
emitTerm scope esc vars yield term =
  let (bound, EmitState graph prev escapes)
        = flip runReader scope
        . flip runStateT EmitState
          { emitGraph  = vars
          , emitPrev   = mempty
          , emitEscape = esc
          }
        $ emitExpr (fromVar vReturn) yield term >> emitVarBinds (fromVar vReturn)

  in ( fst $ flushGraph vReturn prev graph
     , bound
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
          { emitGraph  = vars
          , emitPrev   = mempty
          , emitEscape = esc
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
    g' = foldr (VarMap.delete . toVar) g (emitBinds node)
  in VarSet.foldr
     (\v (s, g) -> first (s<>) (flushGraph v mempty g))
     (mempty, g') (emitDeps node <> extra)

-- | Extract the statements from this node.
nodeStmts :: EmittedNode a -> Seq LuaStmt
nodeStmts EmittedUpvalue{} = mempty
nodeStmts EmittedStmt { emitStmts = s } = s
nodeStmts EmittedExpr { emitExprs = es, emitYield = yield } = yieldStmt yield es

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
emitExpr :: ( Occurs a
            , MonadReader (EmitScope a) m
            , MonadState (EmitState a) m )
         => a -> EmitYield a -> AnnTerm VarSet.Set a
         -> m ()

emitExpr var yield (AnnLet _ (One (v, ty, e)) r) = do
  let yield' = if usedWhen v == Dead then YieldDiscard else YieldDeclare v ty
  emitExpr v yield' e
  local (\s -> s { emitArity = extendPureLets (emitArity s) [(v, ty, e)] }) $
    emitExpr var yield r

emitExpr var yield (AnnLet _ (Many vs) r) =
  local (\s -> s { emitArity = extendPureLets (emitArity s) vs }) $ do
    binds <- traverse (\(v, ty, _) -> (v,) <$> genVars pushScope v ty) vs
    stmt <- traverse (\((_, _, e), (_, v')) -> emitLifted (YieldStore v') e) (zip vs binds)

    graph <- gets emitGraph
    prev  <- gets emitPrev
    arity <- asks emitArity
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
        graph' = foldr (\(v, v') -> VarMap.insert (toVar v) (EmittedStmt stmts v' vs' deps)) graph binds

    -- Update the graph and impure set if needed
    modify (\s -> s { emitGraph = graph'
                    , emitPrev = if p then prev else vss })

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
    graph' <- patternGraph test test' arm =<< gets emitGraph
    modify (\s -> s { emitGraph = graph' })

    -- Emit the body within the existing context
    emitExpr var yield body

  where getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

emitExpr var yield (AnnMatch _ test arms) = do
  (yield', stmt, bound) <- case yield of
    YieldReturn -> pure (YieldReturn, mempty, mempty)
    YieldDiscard -> pure (YieldDiscard, mempty, mempty)
    YieldStore vs -> pure (YieldStore vs, mempty, vs)
    YieldDeclare v ty -> do
      v' <- genVars pushScope v ty
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
      (freeInAtom test,) . Right . LuaIfElse <$> for arms (genBranch test' yield')

  let node = case body of
        Left es -> EmittedExpr [es] yield
        Right ss -> EmittedStmt (stmt |> ss) bound

  modify (\s -> s
    { emitGraph = VarMap.insert (toVar var) (node
                    [var]
                    (deps <> foldMap (extractAnn . _armBody) arms <> emitPrev s))
                  (emitGraph s)
    -- For the time being, we just assume this expression is impure.
    , emitPrev = VarSet.singleton (toVar var)
  })

  where
    genBranch test' yield' arm@Arm { _armPtrn = p, _armBody = b } = do
      graph <- patternGraph test test' arm =<< liftedGraph (extractAnn b)
      scope <- ask
      escape <- gets emitEscape

      let b' = fst3 $ emitTerm scope escape graph yield' b
      pure (patternTest escape p test', toList b')

    genIf yield ifs els = do
      (deps, test') <- runNES (freeInAtom test) (emitAtomS test)

      ifs' <- emitLiftedES yield ifs
      els' <- emitLiftedES yield els
      pure . (deps,) $ case (ifs', els') of
        -- Handle binary ops. Note we don't need to ensure e is not an
        -- unboxed tuple, as we know this returns a boolean.
        (Left [LuaTrue], Left [e]) -> Left $ LuaBinOp test' "or" e
        (Left [e], Left [LuaFalse]) -> Left $ LuaBinOp test' "and" e
        _ -> Right $ LuaIf test' (eitherStmts yield ifs') (eitherStmts yield els')

    eitherStmts _ (Right ss) = toList ss
    eitherStmts yield (Left es) = toList $ yieldStmt yield es

-- Trivial terms. These will just be emitted inline.
emitExpr var yield t@(AnnAtom _ x)    = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnTyApp _ x _) = withinExpr var yield t $ emitAtom x
emitExpr var yield t@(AnnCast _ x _)  = withinExpr var yield t $ emitAtom x

emitExpr var yield t@(AnnApp _ f e) = withinExpr var yield t $ do
  e' <- emitAtom e
  f' <- emitAtomS f

  esc <- gets (emitEscape . nodeState)
  pure
    [ case f' of
        -- Attempt to reduce applications of binary functions to the operators
        -- themselves.
        LuaRef (LuaName op)
          | Just op' <- getEscaped op esc, Just opv <- VarMap.lookup (op' :: CoVar) ops
          , [l, r] <- e'
            -> LuaBinOp l opv r
        _ -> LuaCallE (LuaCall f' e') ]

emitExpr var yield (AnnLam _ TypeArgument{} b) = emitExpr var yield b

emitExpr var yield t@(AnnLam fv (TermArgument v ty) e) = do
  scope <- ask
  graph <- liftedGraph fv
  escape <- gets emitEscape

  let (v', escape') = pushVar v escape
      Identity vs = genVars (const (pure v')) v ty
      graph' = VarMap.insert (toVar v) (EmittedUpvalue vs [v] mempty) graph

      term :: [LuaExpr] = pure . LuaFunction vs . toList . fst3 $ emitTerm scope escape' graph' YieldReturn e
  withinExpr var yield t (pure term)

emitExpr var yield t@(AnnValues _ xs) =
  {-
    There really isn't much we can do as far as unboxed tuples go. We
    just build up a list of all dependent expressions and statement.
  -}
  withinExpr var yield t $
    foldrM (\e es -> (:es) <$> emitAtomS e) mempty xs

emitExpr var yield t@(AnnExtend _ (Lit RecNil) fs) =
  {-
    Record literals are nice and simple to generate, and can just be
    emitted as expressions.

    TODO: Choose an optimal ordering of expressions
  -}
  withinExpr var yield t $
    pure . LuaTable <$> foldrM emitRow mempty fs
  where emitRow (f, _, e) es = (:es) . (LuaString f,) <$> emitAtomS e

emitExpr var yield (AnnExtend fv tbl exs) = do
  {-
    Record extensions have to be emitted as a statement. We build up a
    statement list first, and then.

    TODO: Choose an optimal ordering of expressions and potentially
    inline some statements.
  -}

  (deps, node) <- runNES fv $ case yield of
    YieldDiscard -> do
      exs' <- traverse (emitAtom . thd3) exs
      tbl' <- emitAtom tbl
      pure $ EmittedStmt (foldMap (foldMap asStmt) (tbl':exs')) []

    YieldStore vs -> do
      let vs' = head vs
      exs' <- foldrM (emitRow vs') mempty exs
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaAssign vs [LuaTable []] <| emitCopy vs' tbl' <> exs') vs

    YieldReturn -> do
      let vs' = LuaName (T.pack "__n")
      exs' <- foldrM (emitRow vs') mempty exs
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaLocal [vs'] [LuaTable []] <| emitCopy vs' tbl' <> exs') []

    YieldDeclare var' _ -> do
      vs' <- LuaName <$> pushScope' var'
      exs' <- foldrM (emitRow vs') mempty exs
      tbl' <- emitAtomS tbl
      pure $ EmittedStmt (LuaLocal [vs'] [LuaTable []] <| emitCopy vs' tbl' <> exs') [vs']

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
withinExpr var yield term m = do
  ari <- asks emitArity
  prev <- gets emitPrev

  let p = isPure ari term
      deps = extractAnn term <> (if p then mempty else prev)
  (deps', result) <- runNES deps m

  -- Update the pure set if needed
  unless p (modify (\s -> s { emitPrev = VarSet.singleton (toVar var) }))

  pushGraph var (EmittedExpr result yield [var] deps')

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
  existing <- gets (VarMap.lookup v . emitGraph . nodeState)
  case existing of
    -- Statements and upvalues are easy: the dependency is already in the
    -- graph so we don't need to do any checks.
    Nothing -> pure . LuaRef . LuaName <$> gets (getVar v . emitEscape . nodeState)
    Just EmittedStmt { emitBound = vs } -> pure (map LuaRef vs)
    Just EmittedUpvalue { emitBound = vs } -> pure (map LuaRef vs)

    Just (EmittedExpr expr (YieldDeclare var ty) binds deps) | usedWhen var == Once -> do
      s <- get
      let var' = toVar var
          testEmitGraph' = VarMap.delete var' (emitGraph . nodeState $ s)
          deps' = VarSet.delete var' . VarSet.union deps . nodeDeps $ s
      case hasLoop (VarSet.singleton var') deps' testEmitGraph' of
        Nothing -> do
          (stmts, vs) <- genDeclare pushScope' var ty expr
          let existing' = EmittedStmt { emitStmts = stmts
                                      , emitBound = vs
                                      , emitBinds = binds
                                      , emitDeps  = deps
                                      }
              emitted' = VarMap.insert var' existing' . emitGraph . nodeState $ s
          -- TODO: Maybe convert to lenses?
          modify (\s -> s { nodeState = (nodeState s) { emitGraph = emitted' } })
          pure (map LuaRef vs)
        Just{} -> do
          modify (\s -> s { nodeDeps = deps'
                          , nodeState = (nodeState s) { emitGraph = testEmitGraph' }
                          })
          pure expr
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, bound) <- genYield pushScope' yield expr
      modify (\s -> let graph = VarMap.insert v (EmittedStmt stmts bound binds deps) (emitGraph . nodeState $ s)
                    in s { nodeState = (nodeState s) { emitGraph = graph } })
      pure (map LuaRef bound)
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
                                 (emitDeps e)
                                 (foldr (VarMap.delete . toVar) remaining (emitBinds e))

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
emitAtomMany (Ref v _) = map LuaRef <$> emitVarBinds (toVar v)

-- | Emit the appropriate bindings for a variable.
--
-- This unconditionally promotes an expression to a statement, returning
-- which variables it was bound to.
emitVarBinds :: forall a m.
                ( IsVar a
                , MonadState (EmitState a) m)
             => CoVar -> m [LuaVar]
emitVarBinds v = do
  existing <- gets (VarMap.lookup v . emitGraph)
  case existing of
    -- Statements and upvalues are easy: the dependency is already in the
    -- graph so we don't need to do any checks.
    Nothing -> pure . LuaName <$> gets (getVar v . emitEscape)
    Just EmittedStmt { emitBound = vs } -> pure vs
    Just EmittedUpvalue { emitBound = vs } -> pure vs
    Just (EmittedExpr expr yield binds deps) -> do
      (stmts, vs) <- genYield pushScope yield expr
      pushGraph v EmittedStmt { emitStmts = stmts
                              , emitBound = vs
                              , emitBinds = binds
                              , emitDeps  = deps }
      pure vs

-- | Generate the appropriate code for the provided yield.
genYield :: Monad m
          => (a -> m T.Text) -> EmitYield a -> [LuaExpr]
          -> m (Seq LuaStmt, [LuaVar])
genYield _ YieldReturn es = pure (pure (LuaReturn es), [])
genYield _ YieldDiscard es = pure (foldMap asStmt es, [])
genYield _ (YieldStore vs) es = pure (pure (LuaAssign vs es), vs)
genYield e (YieldDeclare v ty) es = genDeclare e v ty es

-- | Emit a declaration for a variable and a collection of expressions
--
-- This returns a 'LuaLocal' statement binding such expressions and the
-- variables which were bound. We do not emit bindings for expressions
-- which are just variables, returning variable directly.
genDeclare :: Monad m
            => (a -> m T.Text) -> a -> Type a -> [LuaExpr]
            -> m (Seq LuaStmt, [LuaVar])
genDeclare escape v ty es
  -- If all expressions are variables, then just return them
  | Just vs <- traverse getVar es = pure (mempty, vs)
  -- Unpack tuples into multiple assignments
  | ValuesTy ts <- ty = do
      v' <- escape v
      let (rs, lhs, rhs) = tupleVars v' 1 ts es
      pure (pure (LuaLocal lhs rhs), rs)
  -- Generate @local function@ if needed, as it's a wee bit "nicer".
  | [LuaFunction a b] <- es = do
      v' <- escape v
      let var = LuaName v'
      pure (pure (LuaLocalFun var a b), [var])
  -- Just do a normal binding
  | otherwise = do
      v' <- escape v
      let var = LuaName v'
      pure (pure (LuaLocal [var] es), [var])

  where
    -- | Get the variable from an expression
    getVar (LuaRef v@LuaName{}) = Just v
    getVar _ = Nothing

    -- | Generate a set of bindings for a tuple variable
    tupleVars :: T.Text -> Int -> [Type a] -> [LuaExpr]
            -> ([LuaVar], [LuaVar], [LuaExpr])
    tupleVars _ _ [] es = ([], [], es)
    tupleVars v n (_:ts) [] =
      let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
          var = LuaName (v <> T.pack ('_':show n))
      in ( var : rs, var : lhs, rhs)
    tupleVars v n (_:ts) (LuaRef var@LuaName{}:es) =
      let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
      in ( var : rs, lhs, rhs)
    tupleVars v n (_:ts) (e:es) =
      let (rs, lhs, rhs) = tupleVars v (n + 1) ts es
          var = LuaName (v <> T.pack ('_':show n))
      in (var : rs, var : lhs, e   : rhs)

-- | Generate the variables needed for this binding
--
-- This is effectively a simplified version of 'genDeclare' when you do
-- not know the RHS.
genVars :: Monad m
            => (a -> m T.Text) -> a -> Type a
            -> m [LuaVar]
genVars es v (ValuesTy vs) = do
  v' <- es v
  let go _ [] = []
      go n (_:ts) = LuaName (v' <> T.pack ('_':show n)) : go (n + 1) ts
  pure (go (1 :: Int) vs)
genVars es v _ = pure . LuaName <$> es v

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
  modify (\s -> s { topArity = extendForeign (topArity s) (n, t)
                  , topVars = VarMap.insert (toVar n) [LuaName n'] (topVars s) })

  let ex =
        case parseExpr (SourcePos "_" 0 0) (s ^. lazy) of
          Right x -> x
          Left _ -> LuaBitE s
  (LuaLocal [LuaName n'] [ex]<|) <$> emitStmt xs

emitStmt (Type _ cs:xs) = do
  stmts <- foldr (<|) mempty <$> traverse emitConstructor cs
  modify (\s -> s { topArity = extendPureCtors (topArity s) cs })
  (stmts<>) <$> emitStmt xs

  where
    emitConstructor (var, ty) = do
      var' <- pushTopScope var
      modify (\s -> s { topVars = VarMap.insert (toVar var) [LuaName var'] (topVars s) })

      pure $
        if arity ty == 0
        then LuaLocal [LuaName var'] [LuaTable [(LuaString "__tag", LuaString var')]]
        else LuaLocalFun (LuaName var') [LuaName "x"]
                                        [LuaReturn [LuaTable [ (LuaString "__tag", LuaString var')
                                                             , (LuaInteger 1, LuaRef (LuaName "x"))]]]

emitStmt (StmtLet (One (v, ty, e)):xs) = do
  TopEmitState { topArity = ari, topEscape = esc, topVars = vars } <- get
  let yield = if usedWhen v == Dead then YieldDiscard else YieldDeclare v ty
      (stmts, binds, esc') =
        emitTerm (EmitScope ari) esc
                 (VarMap.mapWithKey (\v x -> EmittedUpvalue x [fromVar v] mempty) vars)
                 yield e

  modify (\s -> s { topArity  = extendPureLets (topArity s) [(v, ty, e)]
                  , topVars   = VarMap.insert (toVar v) binds (topVars s)
                  , topEscape = esc' })
  (stmts<>) <$> emitStmt xs

emitStmt (StmtLet (Many vs):xs) = do
  binds <- traverse (\(v, ty, _) -> genVars pushTopScope v ty) vs
  modify (\s -> s { topArity = extendPureLets (topArity s) vs
                  , topVars = foldr (\(v, b) -> VarMap.insert (toVar . fst3 $ v) b) (topVars s) (zip vs binds) })

  TopEmitState { topArity = ari, topEscape = esc, topVars = vars } <- get

  let graph :: EmittedGraph a = VarMap.mapWithKey (\v x -> EmittedUpvalue x [fromVar v] mempty) vars
      (stmt, esc') = foldl' (\(s, esc') ((_, _, e), v') ->
        let (s', _, esc'') = emitTerm (EmitScope ari) esc' graph (YieldStore v') e
        in (s <> s', esc'')) (mempty, esc) (zip vs binds)

  -- We don't strictly need to share escape scopes across expressions, but it helps us avoid shadowing
  modify (\s -> s { topEscape = esc' })

  xs' <- emitStmt xs
  pure $ case stmt of
    (LuaAssign [v] [LuaFunction args bod] :<| Empty) -> LuaLocalFun v args bod <| xs'
    _ -> LuaLocal (mconcat binds) [] <| stmt <> xs'

-- | Push a new node into the emitting graph
pushGraph :: (IsVar b, MonadState (EmitState a) m) => b -> EmittedNode a -> m ()
pushGraph var node = modify (\s -> s { emitGraph = VarMap.insert (toVar var) node (emitGraph s) })

-- | Push a variable into the current scope
pushScope :: (IsVar a, MonadState (EmitState a) m) => a -> m T.Text
pushScope v = state (\s -> let (v', s') = pushVar v (emitEscape s)
                           in (v', s { emitEscape = s' }))

-- | Push a variable into the current scope
pushTopScope :: (IsVar a, MonadState TopEmitState m) => a -> m T.Text
pushTopScope v = state (\s -> let (v', s') = pushVar v (topEscape s)
                              in (v', s { topEscape = s' }))

-- | Push a variable into the current scope
pushScope' :: (IsVar a, MonadState (NodeEmitState a) m) => a -> m T.Text
pushScope' v = state (\s -> let e = nodeState s
                                (v', s') = pushVar v (emitEscape e)
                            in (v', s { nodeState = e { emitEscape = s' } }))

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
  let patDeps = freeInAtom test
  foldrM (\(v, expr) g -> do
    node <- if usedWhen v == Once
            then pure $ EmittedExpr expr (YieldDeclare v (getTy v))
            else uncurry EmittedStmt <$> genDeclare pushScope v (getTy v) expr
    -- TODO: Group declarations where possible?
    pure (VarMap.insert (toVar v) (node [v] patDeps) g))
    graph (patternBindings p test')

  where getTy v = maybe (error "Cannot find pattern variable") snd (find ((==v) . fst) vs)

patternTest :: forall a. IsVar a => EscapeScope -> Pattern a -> [LuaExpr] ->  LuaExpr
patternTest _ PatWildcard _      = LuaTrue
patternTest _ (PatLit RecNil) _  = LuaTrue
patternTest _ PatExtend{} _      = LuaTrue
patternTest _ PatValues{} _      = LuaTrue
patternTest _ (PatLit l)  [vr]   = LuaBinOp (emitLit l) "==" vr
patternTest s (Constr con) [vr]  = tag s con vr
patternTest s (Destr con _) [vr] = tag s con vr
patternTest _ _ _ = undefined

tag :: IsVar a => EscapeScope -> a -> LuaExpr -> LuaExpr
tag scp con vr = LuaBinOp (LuaRef (LuaIndex vr (LuaString "__tag"))) "==" (LuaString (getVar con scp))

-- | A mapping from Amulet binary operators to their Lua equivalent.
ops :: VarMap.Map T.Text
ops = VarMap.fromList
  [ (vOpAdd, "+"),  (vOpAddF, "+")
  , (vOpSub, "-"),  (vOpSubF, "-")
  , (vOpMul, "*"),  (vOpMulF, "*")
  , (vOpDiv, "/"),  (vOpDivF, "/")
  , (vOpExp, "^"),  (vOpExpF, "^")
  , (vOpLt,  "<"),  (vOpLtF,  "<")
  , (vOpGt,  ">"),  (vOpGtF,  "<")
  , (vOpLe,  "<="), (vOpLeF,  "<=")
  , (vOpGe,  ">="), (vOpGeF,  ">=")

  , (vOpConcat, "..")

  , (vOpEq, "==")
  , (vOpNe, "~=")
  ]

-- | Remap an Amulet binary op to the equivalent Lua operator
remapOp :: IsVar a => a -> T.Text
remapOp v | v'@(CoVar _ n _) <- toVar v = fromMaybe n (VarMap.lookup v' ops)

-- | The default 'EscapeScope' for the backend
escapeScope :: EscapeScope
escapeScope =
  let escaper = basicEscaper keywords
  in flip createEscape escaper
   . ((vError, "error"):)
   . ((vLAZY, "__builtin_Lazy"):)
   . ((vForce, "__builtin_force"):)
   . ((vOpApp, "__builtin_app"):)
   . map (fmap escaper)
   $ VarMap.toList ops
