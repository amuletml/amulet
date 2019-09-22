{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}
module Types.Unify.Magic (magicClass, apps) where

import Control.Monad.Infer

import Syntax.Builtin
import Syntax.Var
import Syntax

import Data.Spanned

type Solver m = SomeReason -> Type Typed -> m (Maybe (Wrapper Typed))
type MonadSolve m = (MonadNamey m, MonadWriter [Constraint Typed] m, MonadChronicles TypeError m)

magicClass :: MonadSolve m => Var Typed -> Maybe (Solver m)
magicClass v
  | v == tyKStrName = Just (solveKnownLit knownStrName knownStrTy knownStrTy' tyKStr tyString)
  | v == tyKIntName = Just (solveKnownLit knownIntName knownIntTy knownIntTy' tyKInt tyInt)
  | v == rowConsName = Just solveRowCons
  | otherwise = Nothing

solveKnownLit :: MonadSolve m
              => Var Resolved -> Type Typed -> (Type Typed -> Type Typed) -> Type Typed -> Type Typed
              -> Solver m
solveKnownLit name ty ty' constraint return blame (TyApp _ str) =
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
solveKnownLit _ _ _ _ _ _ _ = error "kind error in solveKnownString"

solveRowCons :: MonadSolve m => Solver m
solveRowCons blame ty =
  case apps ty of
    [ _, record@TyVar{}, tau, TyLit (LiStr key), new ] | isRows new -> do
      x <- genName
      tell [ ConUnify blame x (TyRows record [ (key, tau )]) new ]
      pure (pure (solution record tau key new))
    [ _, record, tau, TyLit (LiStr key), new ] | isRows record -> do
      x <- genName
      tell [ ConUnify blame x (TyRows record [ (key, tau) ]) new ]
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

apps :: Type Typed -> [Type Typed]
apps = reverse . go where
  go (TyApp f x) = x:go f
  go t = [t]

isRows :: Type p -> Bool
isRows TyExactRows{} = True
isRows TyRows{} = True
isRows _ = False
