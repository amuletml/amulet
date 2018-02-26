{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Fold
  ( dropUselessLet
  ) where

import qualified Data.VarSet as VarSet
import Data.Triple

import Core.Optimise

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
