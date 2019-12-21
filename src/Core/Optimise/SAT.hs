{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

{-
The "Static Argument" transformation. The idea of the SAT as an
optimisation is to reduce the number of arguments that must be passed to
a recursive function, and, subsequently, the argument shuffling noise.

Example:

>  let map @a @b (f : a -> b) (xs : list a -> list b) =
>    match xs with
>    | [] -> [] @a
>    | Cons (x, xs) ->
>      let x = f x
>      let xs = map @a @b f xs
>      Cons (x, xs)

Here, the recursive call to map is always made with the same arguments
@a, @b, f and xs. We call these the "static arguments" of map.
We can lift them out of the recursive path:

>  let map @a @b (f : a -> b) =
>     let rec map_sat (xs : list a -> list b) =
>       let map_shadow @a @b (f : a -> b) (xs : list a -> list b) =
>         map_sat xs
>       match xs with
>       | [] -> [] @a
>       | Cons (x, xs) ->
>         let x = f x
>         let xs = map_shadow @a @b f xs
>         Cons (x, xs)
>     map_sat

Then, the shadow can inline:

>  let map @a @b (f : a -> b) =
>     let rec map_sat (xs : list a -> list b) =
>       match xs with
>       | [] -> [] @a
>       | Cons (x, xs) ->
>         let x = f x
>         let xs = map_sat xs
>         Cons (x, xs)
>     map_sat

Now the only parameter we have to pass in the recursive loop is 'xs'.
-}
module Core.Optimise.SAT (staticArgsPass) where

import Control.Monad.Namey
import Control.Lens

import qualified Data.VarMap as VarMap

import Core.Lower.Basic
import Core.Optimise

import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Data.Maybe
import Data.List


-- | Do the static argument transformation on a whole program.
staticArgsPass :: (MonadNamey m, IsVar a) => [Stmt a] -> m [Stmt a]
staticArgsPass = traverse staticArg_stmt

staticArg_stmt :: (MonadNamey m, IsVar a) => Stmt a -> m (Stmt a)
staticArg_stmt (StmtLet (Many xs))
  | [(var, tau, lam)] <- xs, manifestLams tau lam >= 2 = do
      binding <- doStaticArgs var tau lam
      pure $ StmtLet (Many [binding])
  | otherwise = do
      xs' <- traverse (_3 %%~ staticArg_expr) xs
      pure $ StmtLet (Many xs')

staticArg_stmt (StmtLet (One (var, tau, binding))) = do
  exp <- staticArg_expr binding
  pure $ StmtLet (One (var, tau, exp))

staticArg_stmt x@Foreign{} = pure x
staticArg_stmt x@Type{} = pure x

staticArg_expr :: (MonadNamey m, IsVar a) => Term a -> m (Term a)
staticArg_expr (Let (Many xs) tail)
  | [(var, tau, lam)] <- xs, manifestLams tau lam >= 2 = do
      binding <- doStaticArgs var tau lam
      Let (Many [binding]) <$>
        staticArg_expr tail
  | otherwise = do
    xs' <- traverse (_3 %%~ staticArg_expr) xs
    Let (Many xs') <$> staticArg_expr tail

staticArg_expr (Let (One (var, tau, binding)) tail) = do
  binding <- staticArg_expr binding
  Let (One (var, tau, binding)) <$>
    staticArg_expr tail

staticArg_expr (Match atm arms) = Match atm <$> traverse (armBody %%~ staticArg_expr) arms

staticArg_expr t = pure t

manifestLams :: IsVar a => Type -> Term a -> Int
manifestLams (ForallTy Relevant{} _ t) (Lam TypeArgument{} b) =
  1 + manifestLams t b
manifestLams (ForallTy Irrelevant _ t) (Lam TermArgument{} b) =
  1 + manifestLams t b
manifestLams _ _ = 0

{---------------------------------------------------------------------*
*                    The transformation itself                        *
*---------------------------------------------------------------------}

doStaticArgs :: forall a m. (IsVar a, MonadNamey m) => a -> Type -> Term a -> m (a, Type, Term a)
doStaticArgs the_func the_type the_body =
  if not can_be_done || not worth_it then pure (the_func, the_type, the_body) else do
    worker <- fromVar . mkVal <$> genNameFrom (func_name <> "_sat")
    shadow <- mkShadow worker
    worker_body <- mkWorker shadow

    pure
      ( the_func
      , the_type
      , static_lams $
          Let (Many [(fromVar worker, worker_ty, worker_body)])
            (Atom (Ref worker worker_ty))
        )
  where
    func_name = fromMaybe "" (toVar the_func ^. covarName)

    (binders, innards) = splitLams the_body
    n_lams = length binders

    worker_ty = dropQuantifiers (length static_binders) the_type

    -- See «Dropping call prefixes» below
    all_binders =
        map (sconcat . NE.fromList)
      . transpose
      . map (take n_lams . (++ repeat NonStatic) . flip (zipWith isStatic) binders) $ calls
    calls = dropPrefixes . sortOn length $ findCalls the_func the_body

    static_lams body = foldr mkLam body static_binders where
      mkLam (Static a) = Lam a
      mkLam _ = undefined

    non_static_bndrs =
      map (view _2) . filter ((== NonStatic) . view _1) . zip all_binders $ binders

    static_binders = filter (/= NonStatic) all_binders

    worth_it = any value static_binders where
      value (Static TypeArgument{}) = False
      value (Static TermArgument{}) = True
      value NonStatic = False

    can_be_done = ok all_binders where
      ok (Static _:xs) = ok xs
      ok (NonStatic:xs) = all (== NonStatic) xs
      ok [] = False

    mkWorker shadow_rhs = do
      shadow_name <- fromVar . mkVal <$> genNameFrom (func_name <> "_shadow")
      let bd' = substitute (VarMap.singleton (toVar the_func) shadow_ref) innards
          bd' :: Term a
          shadow_ref = Ref (toVar shadow_name) the_type

      let worker_body :: Term a
          worker_body =
            Let (One (shadow_name, the_type, shadow_rhs)) bd'

      pure (foldr Lam worker_body non_static_bndrs)

    mkShadow worker =
      let go_dynamic args = do
            inside <- mkApps (Ref worker worker_ty) worker_ty args
            pure $ foldr Lam inside args

          go (Static (TypeArgument _ k):xs) = do
            x <- fromVar . mkTyvar <$> genName
            Lam (TypeArgument x k) <$> go xs
          go (Static (TermArgument _ k):xs) = do
            x <- fromVar . mkVal <$> genName
            Lam (TermArgument x k) <$> go xs
          go [] = go_dynamic non_static_bndrs
          go _ = error "NonStatic binder in static_binders"

       in go static_binders

{-
Note «Dropping call prefixes»

findCalls f [ f @a @b c d e ] will return the very unhelpful list

    [ [ @a ]
    , [ @a @b ]
    , [ @a @b c ]
    , [ @a @b c d ]
    , [ @a @b c d e ]
    ] -- possibly out of order

Since all of those correspond to a single saturated call, we drop from
the list any call that corresponds to a prefix of a larger call. Any
calls that remain are legit unsaturated and thus remove all of the
parameters that don't appear from the SAT candidates. But! Consider this:

let rec fn x y z = ...
  let _ = whatever (fn x y)
  fn x y z

This is /still/ SAT'able in the arguments x and y, because the calls end
up looking like

  [ [ x, y ]
  , [ x, y, z ]
  ]

For the purpose of the analysis it doesn't /matter/ if a call is
unsaturated, as long as it's a prefix of another, saturated call.
-}

isStatic :: IsVar a => Core.Optimise.Arg -> Argument a -> Static (Argument a)
isStatic (TyArg (VarTy v')) arg@(TypeArgument v _)
  | toVar v == v' = Static arg
isStatic (TermArg (Ref v' _)) arg@(TermArgument v _)
  | toVar v == v' = Static arg
isStatic _ _ = NonStatic

mkApps :: forall a m. (IsVar a, MonadNamey m) => Atom -> Type -> [Argument a] -> m (Term a)
mkApps at _ [] = pure $ Atom at
mkApps at (ForallTy Irrelevant _ t) (TermArgument x tau:xs) = do
  this_app <- fromVar . mkVal <$> genName
  Let (One (this_app, t, App at (Ref (toVar x) tau))) <$>
    mkApps (Ref (toVar this_app) t) t xs
mkApps at (ForallTy r _ t) (TypeArgument v _:xs) = do
  this_app <- fromVar . mkVal <$> genName
  let t' =
        case r of
          Relevant binder -> substituteInType (VarMap.singleton binder (VarTy (toVar v))) t
          Irrelevant -> t
  Let (One (this_app, t', TyApp at (VarTy (toVar v)))) <$>
    mkApps (Ref (toVar this_app) t') t xs
mkApps _ xs ys = error $ "Type error in mkApps: " ++ show xs ++ " " ++ show ys

dropQuantifiers :: Int -> Type -> Type
dropQuantifiers 0 t = t
dropQuantifiers n (ForallTy _ _ t) = dropQuantifiers (n - 1) t
dropQuantifiers _ _ = error "type error in dropQuantifiers"

dropPrefixes :: Eq a => [[a]] -> [[a]]
dropPrefixes [] = []
dropPrefixes (x:xs)
  | any (x `isPrefixOf`) xs = dropPrefixes xs
  | otherwise = x:dropPrefixes xs

data Static a = Static a | NonStatic
  deriving (Eq, Show, Ord)

instance Eq a => Semigroup (Static a) where
  Static x <> Static y
    | x == y = Static x
    | otherwise = NonStatic
  _ <> _ = NonStatic
