{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
module Core.Optimise.Inline
  ( inlineVariablePass
  ) where

import Control.Monad.Gen

import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.Map.Strict as Map
import Data.VarSet (IsVar(..))
import Data.Triple

import Core.Optimise

limit :: Int
limit = 500

data InlineScope a = InlineScope { scores :: VarMap.Map (Atom a, Int)
                                 , cons :: VarSet.Set }

inlineVariablePass :: (MonadGen Int m, IsVar a) => [Stmt a] -> m [Stmt a]
inlineVariablePass = transS (InlineScope mempty mempty) where
  transS _ [] = pure []
  transS s (x@Foreign{}:xs) = (x:) <$> transS s xs
  transS s (StmtLet vars:xs) = do
    vars' <- traverse (third3A (transT s)) vars
    xs' <- transS (extendVars vars' s) xs
    pure (StmtLet vars':xs')
  transS s (x@(Type _ cases):xs) =
    let s' = s { cons = VarSet.fromList (map (toVar . fst) cases) `VarSet.union` cons s }
    in (x:) <$> transS s' xs

  transA _ t@Ref{} = pure t
  transA _ t@Lit{} = pure t
  transA s (Lam arg b) = Lam arg <$> transT s b

  transT :: (MonadGen Int m, IsVar a) => InlineScope a -> Term a -> m (Term a)
  transT s (Atom a) = Atom <$> transA s a
  transT s (App f a) = do
    f' <- transA s f
    a' <- transA s a
    case f' of
      Ref r _
        | Just (Lam (TermArgument v t) b, score) <- VarMap.lookup (toVar r) (scores s)
        , score <= limit -> refresh$ Let (One BindValue (v, t, Atom a')) b
      Lam (TermArgument v t) b -> pure $ Let (One BindValue (v, t, Atom a')) b
      _ -> pure (App f' a')
  transT s (Cast f t) = flip Cast t <$> transA s f
  transT s (TyApp f t) = do
    f' <- transA s f
    case f' of
      Ref r _
        | Just (Lam (TypeArgument v _) b, score) <- VarMap.lookup (toVar r) (scores s)
        , score <= limit -> refresh $ substituteInTys (Map.singleton v t) b
      Lam (TypeArgument v _) b -> pure $ substituteInTys (Map.singleton v t) b
      f' -> pure $ TyApp f' t
  transT s (Extend t rs) = Extend <$> transA s t
                                  <*> traverse (third3A (transA s)) rs
  transT s (Let (One k var) body) = do
    var' <- third3A (transT s) var
    body' <- transT (extendVar var s) body
    pure (Let (One k var') body')
  transT s (Let (Many k vars) body) = do
    vars' <- traverse (third3A (transT s)) vars
    body' <- transT (extendVars vars' s) body
    pure (Let (Many k vars') body')
  transT s (Match test branches) = Match <$> transA s test
                                         <*> traverse (third3A (transT s)) branches

  extendVar :: IsVar a => (a, Type a, Term a) -> InlineScope a -> InlineScope a
  extendVar (v, _, e) s = s
    { scores = case e of
        Atom a | isLambda a && not (occursInTerm v e) -> VarMap.insert (toVar v) (a, scoreTerm s e) (scores s)
        _ -> scores s }
  extendVars :: IsVar a => [(a, Type a, Term a)] -> InlineScope a -> InlineScope a
  extendVars vs s = foldr extendVar s vs

  isLambda (Lam TermArgument{} _) = True
  isLambda (Lam TypeArgument{} (Atom b)) = isLambda b
  isLambda _ = False

scoreAtom :: IsVar a => InlineScope a -> Atom a -> Int
scoreAtom s (Ref v _) = if toVar v `VarSet.member` cons s then 0 else 5
scoreAtom _ (Lit _) = 1
scoreAtom s (Lam TypeArgument{} b) = scoreTerm s b
scoreAtom s (Lam TermArgument{} b) = 1 + scoreTerm s b

scoreTerm :: IsVar a => InlineScope a -> Term a  -> Int
scoreTerm s (Atom a) = scoreAtom s a
scoreTerm s (App f x) = scoreAtom s f + scoreAtom s x + 2
scoreTerm s (Let (One _ v) e) = scoreTerm s (thd3 v) + scoreTerm s e
scoreTerm s (Let (Many _ vs) e) = sum (map (scoreTerm s . thd3) vs) + scoreTerm s e
scoreTerm s (Match e bs) = scoreAtom s e + sum (map (scoreTerm s . thd3) bs)
scoreTerm s (Extend e rs) = scoreAtom s e + sum (map (scoreAtom s . thd3) rs)
scoreTerm s (TyApp t _) = scoreAtom s t
scoreTerm s (Cast t _) = scoreAtom s t
