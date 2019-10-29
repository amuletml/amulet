{-# LANGUAGE FlexibleContexts #-}
module Types.Infer.Function
  ( checkValidTypeFunction
  , makeTypeFunctionHIT
  , checkTypeFunTotality
  )
  where

import Control.Monad.Infer
import Control.Applicative
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Foldable
import Data.Maybe

import Types.Infer.Builtin (checkWildcard)
import Types.Unify (unifyPure, skolFreeTy)

import Syntax.Toplevel
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax

checkValidTypeFunction :: MonadInfer Typed m
                       => SomeReason -> Var Typed -> Type Typed -> [TyConArg Typed] -> [TyFunClause Typed] -> m ()
checkValidTypeFunction _ _ _ _ equations =
  do
    for_ equations $ \clause@(TyFunClause lhs rhs _) -> do
      familyFree (BecauseOf clause) lhs
      checkWildcard clause lhs
      checkWildcard clause rhs
    overlap mempty equations
    terminates equations
    pure ()

overlaps :: Type Typed -> TyFunClause Typed -> Bool
overlaps a (TyFunClause lhs _ _) = a `matches` lhs

matches :: Type Typed -> Type Typed -> Bool
matches a b = isJust (unifyPure a b)

overlap :: MonadInfer Typed m => [TyFunClause Typed] -> [TyFunClause Typed] -> m ()
overlap acc (clause@(TyFunClause lhs rhs ann):xs) = do
  (lhs, sub) <- skolFreeTy mempty (ByInstanceHead lhs (fst ann)) lhs
  rhs <- pure $ apply sub rhs
  case find (overlaps lhs) acc of
    Just (TyFunClause _ their_rhs their_ann)
      | rhs `matches` their_rhs -> overlap (clause:acc) xs
      | otherwise -> dictates (Overlap (Overeq False) lhs (fst their_ann) (fst ann))
    Nothing -> overlap (clause:acc) xs
overlap _ [] = pure ()

familyFree :: MonadInfer Typed m => SomeReason -> Type Typed -> m ()
familyFree what tau = do
  info <- view tySyms
  let uni = universe tau
      fam (TyApps (TyCon v) _) | Just _ <- Map.lookup v info = True
      fam _ = False
  case find fam uni of
    Just t -> confesses (TyFunInLhs what t)
    _ -> pure ()

terminates, checkTypeFunTotality :: MonadInfer Typed m => [TyFunClause Typed] -> m ()
terminates = traverse_ go where
  go clause@(TyFunClause lhs rhs _) =
    let TyApps (TyCon con) argv = lhs
        argvs = recursiveCall con rhs
     in case argvs of
       [] -> pure ()
       (call:_) ->
         unless (all (any (uncurry (~<)) . zip argv) argvs) $
           dictates (MightNotTerminate clause lhs (TyApps (TyCon con) call))

checkTypeFunTotality = terminates

(~<) :: Type Typed -> Type Typed -> Bool
a ~< TyVar b = b `Set.member` ftv a && a /= TyVar b
-- ↑ E.g.: S 'a ~< 'a
TyApps head xs@(_:_) ~< term = term `elem` (head:xs)
-- ↑ E.g.: 'f int ~< int
_ ~< _ = False

recursiveCall :: Var Typed -> Type Typed -> [[Type Typed]]
recursiveCall c (TyApps (TyCon c') args)
  | c == c' = [args] | otherwise = concatMap (recursiveCall c) args
recursiveCall c (TyTuple a b) = recursiveCall c a <|> recursiveCall c b
recursiveCall c (TyTupleL a b) = recursiveCall c a <|> recursiveCall c b
recursiveCall c (TyPi b r) =
  recursiveCall c r <|> case b of
    Anon t -> recursiveCall c t
    Implicit t -> recursiveCall c t
    Invisible _ k _ -> foldMap (recursiveCall c) k
recursiveCall c (TyRows t ts) = recursiveCall c t <> foldMap (recursiveCall c . snd) ts
recursiveCall c (TyExactRows ts) = foldMap (recursiveCall c . snd) ts
recursiveCall c (TyWildcard t) = foldMap (recursiveCall c) t
recursiveCall c (TyParens t) = recursiveCall c t
recursiveCall c (TyApp f x) = recursiveCall c f <|> recursiveCall c x
recursiveCall _ TyCon{} = []
recursiveCall _ TyVar{} = []
recursiveCall _ TyLit{} = []
recursiveCall _ TyPromotedCon{} = []
recursiveCall _ TySkol{} = []
recursiveCall _ TyType{} = []
recursiveCall _ TyOperator{} = undefined
recursiveCall _ TyWithConstraints{} = undefined

makeTypeFunctionHIT :: MonadInfer Typed m => [TyConArg Typed] -> [TyFunClause Typed] -> m [Constructor Typed]
makeTypeFunctionHIT args = traverse go where
  go (TyFunClause lhs rhs ann) = do
    name <- genName
    let (TyApps tyfun args') = lhs
        argtvs = map (\(TyAnnArg a _) -> a) args
        needs = zipWith (\a b -> TyApps tyEq [b, TyVar a]) argtvs args'
        tau = foldr TyArr (TyApps tyEq [TyApps tyfun (map TyVar argtvs), rhs]) needs
        fv = Set.toList (ftv tau)
        closed = foldr (\v -> TyPi (Invisible v (Just TyType) Spec)) tau fv
    pure (GadtCon Public name closed ann)
