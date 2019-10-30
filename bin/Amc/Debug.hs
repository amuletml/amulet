{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Amc.Debug
  ( DebugMode(..)
  , dumpCore
  , dumpTypes
  , dumpCallbacks
  ) where

import Control.Lens

import Control.Monad.Infer
import Syntax.Types
import Syntax

import Core.Core (Stmt)
import Core.Var

import qualified Frontend.Driver as D
import Frontend.Errors

import Backend.Lua

import Text.Pretty.Semantic
import qualified Text.Pretty.Ansi as A

data DebugMode = Void | Test | TestTc
  deriving (Show, Eq)

dumpCallbacks :: DebugMode -> D.DriverCallbacks
dumpCallbacks kind = D.defaultCallbacks { D.onTyped = onTyped } where
  onTyped :: FilePath -> Maybe ([Toplevel Typed], Env, Env) -> ErrorBundle -> IO ()
  onTyped _ Nothing _ = pure ()
  onTyped path (Just (ast, env, penv)) _ = do
    unless (kind == Void) . A.putDoc . annotate (A.BrightColour A.Green) $ "(* Checked:" <+> string path <+> "*)"
    dumpTypes kind ast penv env

dumpTypes :: DebugMode
          -> [Toplevel Typed]
          -> Env -- ^ Old env
          -> Env -- ^ New env
          -> IO ()
dumpTypes Void _ _ _ = pure ()
dumpTypes _ ast penv env = do
    A.putDoc . annotate (A.BrightColour A.Green) $ "(* Program: *)"
    putDoc (pretty ast)
    A.putDoc . annotate (A.BrightColour A.Green) $ "(* Type inference: *)"
    ifor_ (difference env penv ^. names . to toMap) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
      putDoc (pretty k <+> colon <+> displayType t)

dumpCore :: DebugMode
         -> [Stmt CoVar] -- ^ Core
         -> [Stmt CoVar] -- ^ Optimised code
         -> LuaStmt
         -> IO ()
dumpCore Void _ _ _ = pure ()
dumpCore TestTc _ _ _ = pure ()
dumpCore Test core optm lua = do
  A.putDoc . annotate (A.BrightColour A.Green) $ "(* Core lowering: *)"
  putDoc (pretty core)
  A.putDoc . annotate (A.BrightColour A.Green) $ "(* Optimised: *)"
  putDoc (pretty optm)
  A.putDoc . annotate (A.BrightColour A.Green) $ "(* Compiled: *)"
  putDoc (pretty lua)
