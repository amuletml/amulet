{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
{-| The unboxing pass attempts to convert curried functions which take
  multiple arguments to uncurried functions taking unboxed tuples.

  This is performed through a worker/wrapper transformation. We create a
  worker function which takes its arguments as an unboxed tuple, which
  contains the original method's body. We then replace the original
  function with a wrapper, which takes curried arguments (and so has the
  original type signature).

  The idea here is that we do not need to perform complex changes to the
  rest of the code: we instead allow the inliner to handle the
  (relatively) simple wrappers as appropriate.

  == Example
  === Before
  > let f = ᴧ a. λ x. λ y. λ z. EXPR

  === After
  > (* Worker *)
  > let f1 = ᴧ a. λ t1.
  >   match b with (# x, y, z #) ->
  >     EXPR

  > (* Wrapper *)
  > let f  = ᴧ a1. λ x1. λ y1. λ z1.
  >   let t2 = f1 {a1}
  >   let t3 = (# x1, y1, z1 #)
  >   t2 t3
-}
module Core.Optimise.Unbox (unboxPass) where

import Control.Monad.Namey
import Control.Lens ((%%~))
import Control.Arrow

import qualified Data.VarMap as VarMap
import Data.Sequence ((|>), Seq(..))
import qualified Data.Sequence as Seq
import Data.Foldable
import Data.Triple

import Core.Optimise

type Bind a = (a, Type a, Term a)

-- | Runs the unbox pass on the provided statements.
--
-- We need a fresh source of variables for workers and temporary
-- variables, hence the 'MonadNamey'.
unboxPass :: forall a m. (MonadNamey m, IsVar a) => [Stmt a] -> m [Stmt a]
unboxPass = transS where
  transS [] = pure []
  transS (StmtLet (One var):ss) = do
    var' <- third3A transT var
    wvar' <- buildBind var'
    case wvar' of
      Nothing -> (StmtLet (One var'):) <$> transS ss
      Just (worker, wrapper) -> do
        ss' <- transS ss
        pure (StmtLet (One worker) : StmtLet (One wrapper) : ss')

  -- Nested lets
  transS (StmtLet (Many vs):ss) = do
    vs' <- buildBinds vs
    ss' <- transS ss
    pure (StmtLet (Many vs'):ss')

  transS (s:ss) = (s:) <$> transS ss

  -- Trivial terms
  transT t@Atom{} = pure t
  transT t@App{} = pure t
  transT t@TyApp{} = pure t
  transT t@Cast{} = pure t
  transT t@Extend{} = pure t
  transT t@Values{} = pure t

  -- Nested terms
  transT (Match t bs) = Match t <$> traverse (armBody %%~ transT) bs
  transT (Lam arg b) = Lam arg <$> transT b

  -- Trivial lets
  transT (Let (One var) r) = do
    var' <- third3A transT var
    wvar' <- buildBind var'
    case wvar' of
      Nothing -> Let (One var') <$> transT r
      Just (worker, wrapper) -> Let (One worker) . Let (One wrapper) <$> transT r

  -- Nested lets
  transT (Let (Many vs) r) = do
    vs' <- buildBinds vs
    Let (Many vs') <$> transT r

  buildBinds :: [Bind a] -> m [Bind a]
  buildBinds binds = do
    (binds', extras) <- unzip <$> traverse go binds
    pure (mconcat (binds':extras))

    where go b = maybe (b, []) (second pure) <$> buildBind b

  buildBind :: Bind a -> m (Maybe (Bind a, Bind a))
  buildBind (wrapVar, wrapTy, workTerm)
    | isInteresting 0 workTerm = do
        workVar <- freshFrom' wrapVar
        (workTy, workTerm', wrapTerm) <- buildBoxedWrapper workVar wrapTy workTerm
        pure (Just ((workVar, workTy, workTerm'), (wrapVar, wrapTy, wrapTerm)))
    | otherwise = pure Nothing

-- ^ Build a boxed wrapper for some worker function.
buildBoxedWrapper :: forall m a. (IsVar a, MonadNamey m)
                  => a      -- ^ The worker's variable
                  -> Type a -- ^ The wrapper's variable
                  -> Term a -- ^ The original worker's definition
                  -> m (Type a, Term a, Term a)
                  -- ^ The worker's type and definition, plus the wrapper's definition.
buildBoxedWrapper = buildWrapper mempty mempty id mempty mempty mempty id where
  buildWrapper :: Seq (a, Type a) -- ^ The type arguments of the worker function
               -> Seq (a, Type a) -- ^ The value arguments of the worker function
               -> (Term a -> Term a) -- ^ The builder for the worker's body. Used for packing tuples.

               -> VarMap.Map (Type a) -- ^ A set of type variable substitutions to make

               -> Seq (a, Type a)
                  -- ^ The type arguments to be applied to the worker function within the wrapper
               -> Seq (a, Type a)
                  -- ^ The value arguments to be applied to the worker function within the wrapper
               -> (Term a -> Term a) -- ^ The builder for the wrapper's body. Used for unpacking tuples.

               -> a -> Type a -> Term a -- ^ The existing worker var/type/term

               -> m (Type a, Term a, Term a) -- ^ The type and term of the worker, and the wrapper's term
  buildWrapper wkTAs wkVAs wkBuild wrSub wrTAs wrVAs wrBuild bVar bType bTerm =
    case (bType, bTerm) of
      -- Type lambdas are nice and simple: we just add a type application
      -- and wrap the wrapper in a lambda.
      (ForallTy (Relevant _) _ rType, Lam (TypeArgument v ty) rTerm) -> do
        v' <- freshFrom' v
        let ty' = substituteInType wrSub ty

        (workTy, workTerm, wrapTerm) <- buildWrapper
          (wkTAs |> (v, ty)) wkVAs wkBuild
          (VarMap.insert (toVar v) (VarTy v') wrSub) (wrTAs |> (v',ty')) wrVAs wrBuild
          bVar rType rTerm

        pure ( workTy, workTerm
             , Lam (TypeArgument v' ty') wrapTerm )

      -- Term lambdas with unboxed tuples are the complex one: we need to
      -- flatten the tuples, and then repack them for the original
      -- function.
      (ForallTy Irrelevant _ rType, Lam (TermArgument v (ValuesTy tys)) rTerm) -> do
        v' <- freshFrom' v
        vs  <- traverse (const (fromVar <$> fresh ValueVar)) tys
        vs' <- traverse (const (fromVar <$> fresh ValueVar)) tys
        let tys' = map (substituteInType wrSub) tys

        let wkPack = Let (One (v, ValuesTy tys, Values (zipWith Ref vs tys)))

        let wrUnpack b = Match (Ref v' (ValuesTy tys'))
              [ Arm { _armPtrn = PatValues (zipWith Capture vs' tys')
                    , _armTy = ValuesTy tys'
                    , _armBody = b
                    , _armVars = zip vs' tys'
                    , _armTyvars = [] } ]

        (workTy, workTerm, wrapTerm) <- buildWrapper
          wkTAs (wkVAs <> Seq.fromList (zip vs tys)) (wrUnpack . wkBuild)
          wrSub wrTAs (wrVAs <> Seq.fromList (zip vs' tys')) (wkPack . wrBuild)
          bVar rType rTerm

        pure ( workTy, workTerm
             , Lam (TermArgument v' (ValuesTy tys')) wrapTerm )

      -- Basic term lambdas are also simple: add the variables and wrap
      -- it up in a lambda.
      (ForallTy Irrelevant _ rType, Lam (TermArgument v ty) rTerm) -> do
        v' <- freshFrom' v
        let ty' = substituteInType wrSub ty

        (workTy, workTerm, wrapTerm) <- buildWrapper
          wkTAs (wkVAs |> (v, ty)) wkBuild
          wrSub wrTAs (wrVAs |> (v', ty')) wrBuild
          bVar rType rTerm

        pure ( workTy, workTerm
             , Lam (TermArgument v' ty') wrapTerm )

      (_, _) -> do
        argVar <- fresh' ValueVar

        let wkVAs' = toList wkVAs
            argTy = ValuesTy (map snd wkVAs')

            wkBody = Match (Ref argVar argTy)
              [ Arm { _armPtrn = PatValues (map (uncurry Capture) wkVAs')
                    , _armTy = argTy
                    , _armBody = wrBuild bTerm
                    , _armVars = wkVAs'
                    , _armTyvars = [] } ]

            wkLam = foldr (Lam . uncurry TypeArgument)
              (Lam (TermArgument argVar argTy) wkBody)
              wkTAs

            generate ((v, ty) :<| vs) ref = do
              bindVar <- fresh' ValueVar
              (bod, bindTy) <- generate vs bindVar
              let refTy = ForallTy (Relevant v) ty bindTy

              pure ( Let (One (bindVar, bindTy, TyApp (Ref ref refTy) (VarTy v))) bod
                   , refTy )
            generate Empty ref = do
              tupVar <- fresh' ValueVar
              let tupTy = ValuesTy (map snd (toList wrVAs))
                  refTy = ForallTy Irrelevant tupTy (substituteInType wrSub bType)

              pure ( Let (One ( tupVar
                              , argTy
                              , Values (map (uncurry Ref) (toList wrVAs)) ))
                       (App (Ref ref refTy) (Ref tupVar tupTy))
                   , refTy )

        (wrBody, wkTy) <- first wrBuild <$> generate wrTAs bVar
        pure (wkTy, wkLam, wrBody)

-- | Determine if a term has 2 or more variable lambdas and the body is
-- contains something other than a chain of applications.
isInteresting :: IsVar a => Int -> Term a -> Bool
isInteresting n (Lam TypeArgument{} b) = isInteresting n b
isInteresting n (Lam TermArgument{} b) = isInteresting (n + 1) b
isInteresting n body = n >= 2 && not (isWorker Nothing Nothing body)

-- | A vague heuristic to determine if something is a worker
-- function.
--
-- Note this is not complete: we'd rather duplicate worker functions
-- than skip potential optimisation opportunities.
isWorker :: IsVar a => Maybe a -> Maybe a -> Term a -> Bool
-- If we're a type application then we must be applying to the previous
-- function.
isWorker p a (Let (One (v, _, TyApp (Ref f _) _)) body)
  | maybe True (==f) p = isWorker (Just v) a body
-- If we're packing values, ensure it's the only one and mark that as
-- the required argument.
isWorker p Nothing (Let (One (v, _, Values _)) body) =
  isWorker p (Just v) body
-- Skip unpacking values
isWorker p a (Match _ [Arm { _armPtrn = PatValues{}, _armBody = body }]) =
  isWorker p a body
-- The application must be the last expression, applying the previous
-- tyapp with and unboxed tuple
isWorker p (Just a) (App (Ref f _) (Ref x _)) = maybe True (==f) p && x == a
-- If we hit anything else, assume we're not a worker.
isWorker _ _ _ = False
