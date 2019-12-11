{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Amc.Repl.Server
  ( startServer
  , execString
  ) where

import qualified Control.Monad.Infer as T
import Control.Monad.State.Strict
import Control.Exception
import Control.Concurrent
import Control.Lens

import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.VarMap as VarMap

import Data.Traversable
import Data.Position
import Data.Maybe

import qualified Foreign.Lua.Core.Types as L
import qualified Foreign.Lua as L

import System.IO

import qualified Syntax.Var as S

import Core.Var

import qualified Network.Socket as Net

import qualified Backend.Lua.Emit as B

import Text.Pretty.Semantic

import Amc.Repl.State
import Amc.Repl.Eval
import Amc.Repl.Display
import Amc.Repl.Command
import Amc.Debug

-- Aghh MonadFail!
instance MonadFail L.Lua where
  fail x = error $ "MonadFail L.Lua: fail " ++ x

startServer :: MVar () -> Int -> ReplState -> IO ()
startServer ready port state = Net.withSocketsDo $ bracket getSock Net.close (work state) where
  getSock = do
    (addr:_) <-
      Net.getAddrInfo (Just (Net.defaultHints { Net.addrFlags = [ Net.AI_NUMERICHOST
                                                                , Net.AI_PASSIVE
                                                                ]
                                              , Net.addrSocketType = Net.Stream }))
                      (Just "127.0.0.1")
                      (Just (show port))
    sock <- Net.socket Net.AF_INET Net.Stream Net.defaultProtocol
    x <- try $ do
      Net.setSocketOption sock Net.ReuseAddr 1
      Net.bind sock (Net.addrAddress addr)
      Net.listen sock 4
    case x of
      Left (_ :: SomeException) -> do
        putStrLn $ "Failed to start REPL server on port " ++ show port
        killThread =<< myThreadId
      Right () -> pure ()
    putStrLn $ "Listening on port " ++ show port
    putMVar ready ()
    pure sock

  handleReplLine line =
    if | T.null line -> pure ()
       | ':' == T.head line -> uncurry (execCommand Nothing) . span (/=' ') $ T.unpack (T.tail line)
       | otherwise -> () <$ execString "=<remote command>" line

  handleLines handle = do
    eof <- liftIO $ hIsEOF handle
    if eof then
      pure ()
    else do
      line <- liftIO $ T.hGetLine handle
      if T.null line then
        pure ()
      else do
        handleReplLine line
        handleLines handle

  work state sock = do
    (conn, _) <- Net.accept sock
    handle <- Net.socketToHandle conn ReadWriteMode
    hSetEncoding handle utf8

    state <- execStateT (handleLines handle) (state { outputHandle = handle })

    hClose handle

    work state sock

execString :: (MonadState ReplState m, MonadIO m)
           => SourceName -> T.Text
           -> m Bool
execString name line = do
  oldInfer <- gets inferScope
  core <- parseCore parseRepl' name line
  case core of
    Nothing -> pure False
    Just (vs, prog, core) -> do
      (luaExpr, luaSyntax) <- emitCore core
      state <- get
      (ok, res) <- liftIO $ do
        dumpTypes (debugMode (config state)) prog oldInfer (inferScope state)
        dumpCore (debugMode (config state)) core core (pretty luaExpr)

        L.runWith (luaState state) $ do
          L.OK <- L.dostring "-- time out hook\nlocal function f() error('Timed out!', 3) end; debug.sethook(f, '', 1e6)"
          L.OK <- L.loadbuffer luaSyntax ('=':T.unpack name)
          res <- L.try $ L.call 0 L.multret

          case res of
            Right () -> do
              vs' <- for vs $ \(v, _) -> do
                let Just (_, vs) = VarMap.lookup v (emitState state ^. B.topVars)
                repr <- traverse (valueRepr . evalExpr . B.unsimple) vs
                let CoVar id _ _ = v
                    var = S.TgName (covarDisplayName v) id
                case inferScope state ^. T.names . at var of
                  Just _ -> pure (Just (pretty v <+> equals <+> hsep (map pretty repr)))
                  Nothing -> pure Nothing

              pure (True, catMaybes vs')
            Left (L.Exception msg) -> pure (False, [string msg])

      unless (null res) (outputDoc (vsep res))
      pure ok

