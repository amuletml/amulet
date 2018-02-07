{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Fold
  ( foldExpr
  , dropUselessLet
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import qualified Data.Text as Text
import Data.Triple

import Control.Monad.Reader

import Core.Optimise

--- Folds various trivial expressions
foldExpr :: TransformPass
foldExpr = pass go where
  go :: CoTerm -> Trans CoTerm
  go (CotLet [] e) = pure e
  go (CotExtend e []) = pure e

  go e@(CotRef v _) = do
    env <- asks vars
    case Map.lookup v env of
      Just d@(CotRef _ _) -> pure d
      Just d@(CotLit _) -> pure d
      _ -> pure e

  go (CotBegin es e) =
    let es' = foldr (\c r -> case c of
                        CotBegin xs x -> xs ++ x:r
                        CotLit _ -> r
                        CotRef _ _ -> r
                        x -> x:r) [] es
     in pure (if null es' then e else CotBegin es' e)

  go e@(CotApp (CotApp (CotRef (TgInternal v) _) (CotLit ll)) (CotLit rl)) =
    pure $ case (v, ll, rl) of
      ("+",  ColInt l, ColInt r) -> num (l + r)
      ("-",  ColInt l, ColInt r) -> num (l - r)
      ("*",  ColInt l, ColInt r) -> num (l * r)
      ("/",  ColInt l, ColInt r) -> num (l `div` r)
      ("**", ColInt l, ColInt r) -> num (l ^ r)
      ("<" , ColInt l, ColInt r) -> bool (l < r)
      (">",  ColInt l, ColInt r) -> bool (l > r)
      (">=", ColInt l, ColInt r) -> bool (l >= r)
      ("<=", ColInt l, ColInt r) -> bool (l <= r)

      ("&&", ColTrue, ColTrue)   -> bool True
      ("&&", _, _)               -> bool False
      ("||", ColFalse, ColFalse) -> bool False
      ("||", _, _)               -> bool True

      ("^", ColStr l, ColStr r)  -> str (l `Text.append` r)

      _ -> e
  go e = pure e

  num = CotLit . ColInt
  str = CotLit . ColStr
  bool x = CotLit (if x then ColTrue else ColFalse)

dropUselessLet :: TransformPass
dropUselessLet = pass' go where
  go (CotLet vs e) =
    -- First filter our definitions to only include ones
    -- which are referenced
    let keep = buildKeep (freeIn e) vs False []
        vs' = filter (flip VarSet.member keep . fst3) vs
    in case (vs', e) of
      -- If we've no bindings, just return the primary expression
      ([], e) -> e
      -- If we're of the form `let x = y in x`, simplify to `y`.
      ([(v, _, d)], CotRef r _) | v == r -> d
      -- Otherwise emit the full binding
      (vs', e) -> CotLet vs' e

  go x = x

  -- Generate a set of variables to keep, looping until we reach a fixed point
  buildKeep keep [] True rest = buildKeep keep rest False []
  buildKeep keep [] False _   = keep
  buildKeep keep (b@(v, _, d):bs) change rest =
    -- We should only emit a binding if it is impure or used somewhere else
    -- This somewhere else will either be another preserved binding or the let's expression
    if not (pure d) || v `VarSet.member` keep
    then buildKeep (keep `VarSet.union` freeIn d `VarSet.union` VarSet.singleton v) bs True rest
    else buildKeep keep bs change (b:rest)

  pure CotLam{} = True
  pure CotRef{} = True
  pure CotLit{} = True
  pure CotApp{} = False
  pure (CotTyApp f _) = pure f
  pure (CotLet vs e) = pure e && all (pure . thd3) vs
  pure (CotBegin xs e) = all pure xs && pure e
  pure (CotMatch e bs) = pure e && all (pure . thd3) bs
  pure (CotExtend e rs) = pure e && all (pure . thd3) rs
