-- | All available compile targets. This is in a separate module to the
-- main 'CompileTarget' definitions in order to reduce the length of the
-- module dependency chains.

{-# LANGUAGE OverloadedStrings #-}
module CompileTargets (Target, lua, scheme, targets) where

import Data.Bifunctor

import Text.Pretty.Note

import qualified Language.Lua.Parser as Lua
import CompileTarget

lua :: Target
lua = Target "lua" (\pos file -> bimap ParseError (const ()) $ Lua.parseExpr pos file) Lua.highlightLua

scheme :: Target
scheme = Target "scheme" (\_ _ -> pure ()) defaultHighlight

-- | All known backend targets.
targets :: [Target]
targets = [ lua ]
