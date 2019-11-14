{-# LANGUAGE ConstraintKinds, FlexibleContexts, ScopedTypeVariables #-}
module Types.Unify.Magic (magicClass, magicTyFun) where

import Control.Monad.Infer
import Control.Lens

import qualified Data.Sequence as Seq
import Data.Foldable

import Syntax.Implicits
import Syntax.Builtin
import Syntax.Pretty
import Syntax.Types
import Syntax.Var
import Syntax

import Data.Spanned

import Text.Pretty.Semantic

import Types.Unify.Equality
import Types.Unify.Base

type Solver m = SomeReason -> ImplicitScope ClassInfo Typed -> Type Typed -> m (Maybe (Wrapper Typed))
type TfSolver m = ImplicitScope ClassInfo Typed -> [Type Typed] -> Type Typed -> m (Maybe (Coercion Typed))
type Reporter m = SomeReason -> ImplicitScope ClassInfo Typed -> Type Typed -> Seq.Seq TypeError -> m ()

magicClass :: MonadSolve m => Var Typed -> Maybe (Solver m, Reporter m)
magicClass v
  | v == tyKStrName = Just (solveKnownLit knownStrName knownStrTy knownStrTy' tyKStr tyString, confess')
  | v == tyKIntName = Just (solveKnownLit knownIntName knownIntTy knownIntTy' tyKInt tyInt, confess')
  | v == rowConsName = Just (solveRowCons, confess')
  | v == tyEqName = Just (solveEq, confess_eq)
  | v == tyTypeError_n = Just (\_ scope (TyApps _ [a]) -> solveTypeError scope a, confess')
  | otherwise = Nothing

confess' :: MonadSolve m => Reporter m
confess' reason _ ts xs = confesses (UnsatClassCon reason (ConImplicit reason mempty undefined ts) (MagicErrors (toList xs)))

confess_eq :: MonadSolve m => Reporter m
confess_eq reason scope (TyApps _ [a, b]) _ = do
  e <- unequal scope a b
  confesses (ArisingFrom e reason)
confess_eq _ _ _ _ = undefined

solveEq :: MonadSolve m => Solver m
solveEq blame classes ty@(TyApps _ [a, b]) = do
  var <- genName
  tell [ ConUnify blame classes var (TyApps tyEq [a, a]) (TyApps tyEq [a, b]) ]
  let refl = ExprWrapper (Cast (AssumedCo (TyApps tyEq [a, a]) (TyApps tyEq [a, b])))
                (ExprWrapper (TypeApp a) (VarRef rEFLName (span, rEFLTy)) (span, rEFLTy' a))
                (span, ty)
      span = annotation blame
  pure (Just (ExprApp refl))
solveEq _ _ _ = undefined

solveKnownLit :: MonadSolve m
              => Var Resolved -> Type Typed -> (Type Typed -> Type Typed) -> Type Typed -> Type Typed
              -> Solver m
solveKnownLit name ty ty' constraint return blame _ (TyApp _ str) =
  case str of
    TyLit l -> pure $ pure (solution l)
    _ -> pure Nothing
  where
    solution :: Lit -> Wrapper Typed
    solution t = ExprApp $
      App (ExprWrapper (TypeApp (TyLit t))
            (VarRef name (span, ty))
            (span, ty' (TyLit t)))
          (Literal t (span, return))
          (span, TyApp constraint (TyLit t))
    span = annotation blame
solveKnownLit _ _ _ _ _ _ _ _ = error "kind error in solveKnownString"

solveRowCons :: MonadSolve m => Solver m
solveRowCons blame classes ty =
  case appsView ty of
    [ _, record@TyVar{}, TyLit (LiStr key), tau, new ] | isRows new -> do
      x <- genName
      tell [ ConUnify blame classes x (TyRows record [ (key, tau )]) new ]
      pure (pure (solution record tau key new))
    [ _, record, TyLit (LiStr key), tau, new ] | isRows record -> do
      x <- genName
      let (innermost, ks) = getRows record
      case innermost of
        Just t -> tell [ ConUnify blame classes x (TyRows t ((key, tau):(ks `without` key)) ) new ]
        _      -> tell [ ConUnify blame classes x (TyExactRows ((key, tau):(ks `without` key)) ) new ]
      pure (pure (solution record tau key new))
    _ -> pure Nothing
  where
    solution record tau key new =
      let solution =
            App (ExprWrapper (TypeApp new)
                  (ExprWrapper (TypeApp tau)
                    (ExprWrapper (TypeApp record)
                      (ExprWrapper (TypeApp keyt)
                        (VarRef rOWCONSName (span, rowConsTy))
                        (span, rowConsTy' keyt))
                      (span, rowConsTy'' keyt record))
                    (span, rowConsTy''' keyt record tau))
                  (span, rowConsTy'''' keyt record tau new))
                (Literal (LiStr key) (span, tyString))
                (span, ty)
          keyt = TyLit (LiStr key)
       in ExprApp solution
    span = annotation blame
    xs `without` b = filter ((/= b) . fst) xs

    getRows (TyExactRows rs) = (Nothing, rs)
    getRows (TyRows t rs) = getRows t & _2 %~ (rs ++)
    getRows t = (Just t, [])

isRows :: Type p -> Bool
isRows TyExactRows{} = True
isRows TyRows{} = True
isRows _ = False

magicTyFun :: MonadSolve m => Var Typed -> Maybe (TfSolver m)
magicTyFun v
  | v == tyTypeError_n = Just (\scope [a] _ -> solveTypeError scope a)
  | otherwise = Nothing

solveTypeError :: forall m a. MonadSolve m => ImplicitScope ClassInfo Typed -> Type Typed -> m a
solveTypeError scope t = confesses . CustomTypeError =<< toTypeError' t where
  toTypeError, toTypeError' :: Type Typed -> m Doc
  toTypeError' t = toTypeError =<< reduceTyFuns scope (TyParens t)

  toTypeError (TyApps (TyCon v) [a, b])
    | v == tyVCat_n = (<#>) <$> toTypeError' a <*> toTypeError' b
    | v == tyHCat_n = (<+>) <$> toTypeError' a <*> toTypeError' b

  toTypeError (TyApps (TyPromotedCon v) [a])
    | v == tyeString_n = do
      a <- reduceTyFuns scope a
      case a of
        TyLit (LiStr a) -> pure $ text a
        _ -> pure $ displayTypeTyped a
    | v == tyShowType_n = pure $ displayTypeTyped a

  toTypeError (TyParens x) = toTypeError x

  toTypeError _ = pure $ string "Stuck type expression in Amc.type_error:" <+> displayTypeTyped t
