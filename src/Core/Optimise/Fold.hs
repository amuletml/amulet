{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Fold
  ( foldExpr
  , dropUselessLets
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

import Data.Foldable
import Data.Function
import Data.Triple
import Data.Maybe
import Data.List

import Control.Monad.Reader

import Core.Optimise

--- Folds various trivial expressions
foldExpr :: TransformPass
foldExpr = afterPass pass where
  pass :: CoTerm -> TransM CoTerm
  pass (CotLet [] e) = pure e
  pass (CotExtend e []) = pure e

  pass e@(CotRef v _) = do
    env <- vars <$> ask
    case Map.lookup v env of
      Just d@(CotRef _ _) -> pure d
      Just d@(CotLit _) -> pure d
      _ -> pure e

  pass (CotBegin es e) = let es' = foldr (\c r -> case c of
                                                    CotBegin xs x -> xs ++ x:r
                                                    CotLit _ -> r
                                                    CotRef _ _ -> r
                                                    x -> x:r) [] es in
                           pure (if null es' then e else CotBegin es' e)

  pass e@(CotApp (CotApp (CotRef (TgInternal v) _) (CotLit ll)) (CotLit rl)) =
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

  pass e = pure e

  num = CotLit . ColInt
  str = CotLit . ColStr
  bool x = CotLit (if x then ColTrue else ColFalse)


dropUselessLets :: TransformPass
dropUselessLets = afterPass' go where
  go (CotLet gr1 e)
    | inter <- intersectBy ((==) `on` fst3) (sortOn fst3 gr1) (map (\x -> (x, undefined, undefined)) (toList (freeIn e)))
    , diff <- deleteFirstsBy ((==) `on` fst3) gr1 inter
    = case mapMaybe (keep . thd3) diff of
        [] -> CotLet inter e
        xs -> CotBegin xs (CotLet inter e)
    | otherwise = CotLet gr1 e
  go e = e

  keep CotLit{} = Nothing
  keep CotRef{} = Nothing
  keep x = Just x
