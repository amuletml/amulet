{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, FlexibleContexts #-}
module Debug
  ( DebugMode(..)
  , dump
  ) where

import Control.Lens

import Control.Monad.Infer
import Syntax.Pretty
import Syntax.Types

import Core.Core (Stmt)
import Core.Var

import Backend.Lua

import Text.Pretty.Semantic

data DebugMode = Void | Test | TestTc

dump :: DebugMode
     -> [Toplevel Typed]
     -> [Stmt CoVar] -- |Core
     -> [Stmt CoVar] -- |Optimised code
     -> LuaStmt
     -> Env          -- |Base environment
     -> Env          -- |Current environment
     -> IO ()

dump Void _ _ _ _ _ _ = pure ()

dump Test ast core optm lua penv env = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  putDoc (pretty ast)
  putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
  ifor_ (difference env penv ^. values . to toMap) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
    putDoc (pretty k <+> colon <+> displayType t)
  putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
  ifor_ (difference env penv ^. types ^. to toMap) . curry $ \(k :: Var Resolved, t) ->
    putDoc (pretty k <+> colon <+> displayType t)
  putStrLn "\x1b[1;32m(* Core lowering: *)\x1b[0m"
  putDoc (pretty core)
  putStrLn "\x1b[1;32m(* Optimised: *)\x1b[0m"
  putDoc (pretty optm)
  putStrLn "\x1b[1;32m(* Compiled: *)\x1b[0m"
  putDoc (pretty lua)
  pure ()

dump TestTc ast _ _ _ penv env = do
  putStrLn "\x1b[1;32m(* Program: *)\x1b[0m"
  putDoc (pretty ast)
  putStrLn "\x1b[1;32m(* Type inference: *)\x1b[0m"
  ifor_ (difference env penv ^. values . to toMap) . curry $ \(k :: Var Resolved, t :: Type Typed) ->
    putDoc (pretty k <+> colon <+> pretty t)
  putStrLn "\x1b[1;32m(* Kind inference: *)\x1b[0m"
  ifor_ (difference env penv ^. types . to toMap) . curry $ \(k :: Var Resolved, t) ->
    putDoc (pretty k <+> colon <+> pretty t)
