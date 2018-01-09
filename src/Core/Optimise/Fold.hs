{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Fold
  ( foldExpr
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

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
