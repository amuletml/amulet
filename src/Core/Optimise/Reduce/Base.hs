{-# LANGUAGE
  ConstraintKinds
, FlexibleContexts
, ScopedTypeVariables
, TemplateHaskell #-}
module Core.Optimise.Reduce.Base
  ( module Core.Core
  , module Core.Var

  , ReduceScope(..)
  , vars, types, ctors
  , MonadReduce
  , runReduce, runReduceN

  , changing, changed
  , isCtor
  , lookupVar, lookupBaseVar, lookupRawVar, lookupRawTerm
  , unwrapTy
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.Namey
import Control.Arrow
import Control.Lens

import qualified Data.VarMap as VarMap

import Core.Core
import Core.Var

data ReduceScope a
  = ReduceScope
    { _vars :: VarMap.Map (Term a)
    , _types :: VarMap.Map [(a, Type a)]
    , _ctors :: VarMap.Map (Type a) }
  deriving (Show)

makeLenses ''ReduceScope

type MonadReduce a m =
  ( IsVar a
  , MonadNamey m
  , MonadReader (ReduceScope a) m
  , MonadWriter (Sum Int) m
  )

-- | Run the reduce monad in the default scope, returning the modified
-- expression and the number of changes made.
runReduce :: MonadNamey m
          => ReaderT (ReduceScope a) (WriterT (Sum Int) m) x
          -> m (x, Int)
runReduce = (second getSum<$>) . runWriterT . flip runReaderT emptyScope where
  emptyScope = ReduceScope mempty mempty mempty

-- | Run the reduce monad N times, or until no more changes occur.
runReduceN :: MonadNamey m
           => (x -> ReaderT (ReduceScope a) (WriterT (Sum Int) m) x)
           -> Int -> x
           -> m x
runReduceN task = go where
  go 0 x = pure x
  go n x = do
    (x', ch) <- runReduce (task x)
    if ch > 0 then go (n - 1) x' else pure x'

-- | Mark this transformation as having changed the term
changing :: MonadReduce a m => m x -> m x
changing = (tell (Sum 1)>>)

-- | Return a changed term
changed :: MonadReduce a m => x -> m x
changed = (<$tell (Sum 1))

isCtor :: IsVar a => a -> ReduceScope a -> Bool
isCtor var = VarMap.member (toVar var) . view ctors

lookupVar :: IsVar a => a -> ReduceScope a -> Maybe (Term a)
lookupVar v = VarMap.lookup (toVar v) . view vars

lookupBaseVar :: forall a. IsVar a => a -> ReduceScope a -> Maybe (Term a)
lookupBaseVar v s = lookup v v where
  lookup u v = case (var u, var v) of
                 (Just (Atom (Ref u' _)), Just (Atom (Ref v' _)))
                   | Just e@(Atom (Ref v'' _)) <- var v' ->
                     if u' == v''
                     then Just (smallest v'' e)
                     else lookup u' v''
                 (_, t) -> t

  smallest v e = case var v of
                   Just e'@(Atom (Ref v' _)) | v' < v -> e'
                   _ -> e

  var :: a -> Maybe (Term a)
  var = flip VarMap.lookup (s ^. vars) . toVar

lookupRawVar :: IsVar a => a -> ReduceScope a -> a
lookupRawVar v s =
  case lookupBaseVar v s of
    Just (TyApp (Ref v' _) _) -> lookupRawVar v' s
    _ -> v

lookupRawTerm :: IsVar a => a -> ReduceScope a -> Maybe (Term a)
lookupRawTerm v s = VarMap.lookup (toVar (lookupRawVar v s)) (s ^. vars)


  -- | Extract the type name from a constructor's type. This is a little grim.
unwrapTy :: Type a -> Maybe a
unwrapTy (ForallTy _ _ t) = unwrapTy t
unwrapTy (AppTy t _) = unwrapTy t
unwrapTy (ConTy v) = Just v
unwrapTy _ = Nothing
