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

import Syntax (Resolved, Var(..))
import Core.Optimise

--- Folds various trivial expressions
foldExpr :: TransformPass
foldExpr = pass go where
  go :: CoTerm (Var Resolved) -> Trans (CoTerm (Var Resolved))

  -- Empty expressions
  go (CotLet [] e) = pure e
  go (CotExtend e []) = pure (CotAtom e)

  -- Commuting conversion
  go (CotLet [(x, xt, CotLet [(y, yt, yval)] xval)] rest) | x `VarSet.notMember`freeIn yval
    = go $ CotLet [(y, yt, yval)] $ CotLet [(x, xt, xval)] rest

  -- Reduce directly called functions to lambdas
  go (CotApp (CoaLam Small (var, ty) body) ex) = go $ CotLet [(var, ty, CotAtom ex)] body
  go (CotTyApp (CoaLam Big (var, _) body) tp) = go (substituteInTys (Map.singleton var tp) body)

  go e@(CotAtom(CoaRef v _)) = do
    env <- asks vars
    case Map.lookup v env of
      Just d@(CotAtom CoaRef{}) -> go d
      Just d@(CotAtom(CoaLit _)) -> pure d
      _ -> pure e

  go e@(CotApp (CoaRef f1 _) (CoaLit r1)) = do
    env <- asks vars
    pure $ case Map.lookup f1 env of
             Just (CotApp (CoaRef (TgInternal v) _) (CoaLit l1)) ->
               case (v, l1, r1) of
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
             _ -> e
  go e = pure e

  num = CotAtom . CoaLit . ColInt
  str = CotAtom . CoaLit . ColStr
  bool x = CotAtom (CoaLit (if x then ColTrue else ColFalse))

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
      ([(v, _, d)], CotAtom(CoaRef r _)) | v == r -> d
      -- Otherwise emit the full binding
      (vs', e) -> CotLet vs' e

  go x = x

  -- Generate a set of variables to keep, looping until we reach a fixed point
  buildKeep keep [] True rest = buildKeep keep rest False []
  buildKeep keep [] False _   = keep
  buildKeep keep (b@(v, _, d):bs) change rest =
    -- We should only emit a binding if it is impure or used somewhere else
    -- This somewhere else will either be another preserved binding or the let's expression
    if not (isPure d) || v `VarSet.member` keep
    then buildKeep (keep `VarSet.union` freeIn d `VarSet.union` VarSet.singleton v) bs True rest
    else buildKeep keep bs change (b:rest)

  isPure CotAtom{} = True
  isPure CotApp{} = False
  isPure CotTyApp{} = True
  isPure CotExtend{} = True
  isPure (CotLet vs e) = isPure e && all (isPure . thd3) vs
  isPure (CotMatch _ bs) = all (isPure . thd3) bs
