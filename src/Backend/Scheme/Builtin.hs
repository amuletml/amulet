{-# LANGUAGE OverloadedStrings #-}

{-|A collection of all builtin variables which Amulet might use
-}
module Backend.Lua.Builtin
  ( ExtraVars, builtinVars
  , builtinEscape
  , builtinBuilders
  ) where

import qualified Data.VarMap as VarMap
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Data.Sequence (Seq)

import Core.Builtin
import Core.Var

import Language.Lua.Syntax
import Language.Lua.Quote

import Backend.Escape

-- | The basic escaper for Lua code bases
escaper :: T.Text -> T.Text
escaper = basicEscaper keywords

-- | The default 'EscapeScope' for the backend.
builtinEscape :: EscapeScope
builtinEscape
  = flip createEscape escaper
  . ((vError, "error"):)
  $ map (\(v, n, _, _, _) -> (v, n)) builtins

-- | A set of variables which will be lazilly emitted by the backend.
--
-- See "Backend.Emit.Postprocess" for more information.
type ExtraVars = VarMap.Map ([CoVar], Seq Scheme)

-- | The default 'ExtraVars' for the backend.
builtinVars :: ExtraVars
builtinVars = VarMap.fromList $ map (\(v, _, d, _, s) -> (v, (d, Seq.fromList s))) builtins

-- | All native "builders"
builtinBuilders :: VarMap.Map ((Int, [Scheme] -> (Seq Scheme, [Scheme])), LuaVar)
builtinBuilders = foldr go mempty builtins where
  go (_, _, _, Nothing, _) m = m
  go (v, n, _, Just b, _)  m = VarMap.insert v (b, LuaName n) m

builtins :: [( CoVar, T.Text, [CoVar]
             , Maybe (Int, [Scheme] -> (Seq Scheme, [Scheme]))
             , [Scheme] )]
builtins =
  [ ]
