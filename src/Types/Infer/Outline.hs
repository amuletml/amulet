{-# LANGUAGE FlexibleContexts, TupleSections, OverloadedStrings #-}
module Types.Infer.Outline (approximate) where

import Control.Monad.Infer
import Control.Monad.State

import qualified Data.Map.Strict as Map

import Syntax.Subst
import Syntax.Types
import Syntax.Var
import Syntax

import Types.Infer.Builtin
import Types.Kinds

-- For polymorphic recursion, mostly
approxType, approxType' :: MonadInfer Typed m => Expr Desugared -> StateT Origin m (Type Typed)
approxType r@(Ascription _ t _) = resolveKind (becauseExp r) t
approxType ex = do
  put Deduced
  approxType' ex

approxType' r@(Fun p e _) = TyPi <$> approxParam p <*> approxType' e where
  approxParam (PatParam p) = Anon <$> approxPattern r p
  approxParam (EvParam _) = error "approx EvParam"

approxType' r@(Ascription _ t _) = resolveKind (becauseExp r) t
approxType' (Match _ (Arm _ _ e:_) _) = approxType' e
approxType' (If _ t _ _) = approxType' t
approxType' (Begin xs _) = approxType' (last xs)
approxType' (Let _ e _) = approxType' e

approxType' (Literal l _) = case l of
  LiInt _ -> pure tyInt
  LiStr _ -> pure tyString
  LiFloat _ -> pure tyFloat
  LiBool _ -> pure tyBool
  LiUnit -> pure tyUnit

approxType' (Record vs _) = do
  let one (Field r e _) = (r,) <$> approxType' e
  TyExactRows <$> traverse one vs

approxType' (BinOp _ (VarRef o _) _ _) | o `Map.member` builtinOps = pure (builtinOps Map.! o)
approxType' (App (VarRef v _) (Fun (PatParam (PLiteral LiUnit _)) e _) _) | v == TgInternal "lazy" =
  TyApp tyLazy <$> approxType' e

approxType' _ = guess

approximate :: MonadInfer Typed m
            => Binding Desugared
            -> m (Origin, (Var Typed, Type Typed))
approximate (Binding v e _) = do
  (ty, st) <- runStateT (approxType e) Supplied
  ty' <- generalise nominalTvs (becauseExp e) ty
  pure (st, (v, if not (wasGuessed st) then ty' else ty))
approximate Matching{} = error "Matching before TC"
approximate TypedMatching{} = error "TypedBinding before TC"

wasGuessed :: Origin -> Bool
wasGuessed Guessed = True
wasGuessed _ = False

approxPattern :: MonadInfer Typed m => Expr Desugared -> Pattern Desugared -> StateT Origin m (Type Typed)
approxPattern _ Wildcard{} = guess
approxPattern _ Capture{} = guess
approxPattern _ Destructure{} = guess
approxPattern r (PAs p _ _) = approxPattern r p
approxPattern r (PType _ t _) = resolveKind (becauseExp r) t
approxPattern r (PRecord vs _) = do
  let one (l, p) = (l,) <$> approxPattern r p
  v <- freshTV {- not a guess! -}
  TyRows v <$> traverse one vs

approxPattern r (PTuple xs _)
  | length xs >= 2 =
    foldr1 TyTuple <$> traverse (approxPattern r) xs
  | otherwise = guess

approxPattern _ (PLiteral l _) = case l of
  LiInt _ -> pure tyInt
  LiStr _ -> pure tyString
  LiFloat _ -> pure tyFloat
  LiBool _ -> pure tyBool
  LiUnit -> pure tyUnit

approxPattern _ PWrapper{} = error "Impossible PWrapper"
approxPattern _ PSkolem{} = error "Impossible PSkolem"

guess :: MonadInfer Typed m => StateT Origin m (Type Typed)
guess = do
  put Guessed
  lift freshTV

builtinOps :: Map.Map (Var Desugared) (Type Typed)
builtinOps =
  Map.fromList
    [ (TgInternal "+", tyInt)
    , (TgInternal "-", tyInt)
    , (TgInternal "*", tyInt)
    , (TgInternal "/", tyFloat)
    , (TgInternal "**", tyInt)
    , (TgInternal "+.", tyFloat)
    , (TgInternal "-.", tyFloat)
    , (TgInternal "*.", tyFloat)
    , (TgInternal "/.", tyFloat)
    , (TgInternal "**.", tyFloat)
    , (TgInternal "^", tyString)
    , (TgInternal ">", tyBool)
    , (TgInternal "<", tyBool)
    , (TgInternal ">=", tyBool)
    , (TgInternal "<=", tyBool)
    , (TgInternal ">.", tyBool)
    , (TgInternal "<.", tyBool)
    , (TgInternal ">=.", tyBool)
    , (TgInternal "<=.", tyBool)
    , (TgInternal "==", tyBool)
    , (TgInternal "<>", tyBool)
    ]
