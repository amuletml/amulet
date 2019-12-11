{-# LANGUAGE OverloadedStrings, FlexibleContexts, ScopedTypeVariables, TypeFamilies, MultiWayIf #-}

module Amc.Repl.Eval
  ( evalExpr
  , patchupLua
  , wrapDriver
  , wrapNamey
  , completeInScope
  , emitCore
  , outputDoc
  , loadFile
  , parseRepl'
  , parseCore
  ) where

import Control.Monad.State.Strict
import qualified Foreign.Lua.Core.Types as L
import qualified Foreign.Lua as L
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import qualified Backend.Lua.Emit as B
import qualified Backend.Escape as B
import Language.Lua.Syntax
import Core.Var
import Control.Lens
import Amc.Repl.State
import qualified Frontend.Driver as D
import Control.Monad.Namey
import qualified Data.ByteString as Bs
import qualified Backend.Lua.Postprocess as B
import Core.Core (Stmt)

import qualified Data.Text.Lazy as L
import qualified Data.Map.Strict as Map

import Data.Bifunctor
import Data.Foldable
import Data.Position
import Data.Functor
import Data.Spanned
import Data.Triple
import Data.Char

import System.Console.Haskeline hiding (display, bracket, throwTo)
import System.Directory
import System.IO

import Syntax.Resolve (ResolveResult(..))
import Syntax.Resolve.Scope (Signature(..), Slot(..))
import qualified Syntax.Var as S
import qualified Syntax as S

import Control.Timing

import Parser.Wrapper (Parser, runParser)
import Parser

import qualified Core.Core as C
import Core.Occurrence
import Core.Lint

import Frontend.Errors

import Text.Pretty.Semantic

import Amc.Repl.Display
import Amc.Debug

evalExpr :: LuaExpr -> L.Lua ()
evalExpr (LuaRef (LuaName n)) = L.getglobal (T.unpack n)
evalExpr LuaNil = L.pushnil
evalExpr LuaTrue = L.pushboolean True
evalExpr LuaFalse = L.pushboolean True
evalExpr (LuaInteger n) = L.pushinteger (fromIntegral n)
evalExpr (LuaNumber n) = L.pushnumber (L.Number n)
evalExpr (LuaString s) = L.pushstring (T.encodeUtf8 s)
evalExpr s = error ("Not a simple expression: " ++ show s)

-- | We convert any top-level local declarations into global ones. This
-- means they are accessible outside normal REPL invocations.
patchupLua :: B.TopEmitState -> LuaStmt -> LuaStmt
patchupLua s (LuaLocal vs [])
  | all (isTopVar s) vs
  = LuaAssign vs [LuaNil]
patchupLua s (LuaLocal vs es)
  | all (isTopVar s) vs
  = LuaAssign vs es
patchupLua s (LuaLocalFun v as ss)
  | isTopVar s v
  = LuaAssign [v] [LuaFunction as ss]
patchupLua _ x = x

isTopVar :: B.TopEmitState -> LuaVar -> Bool
isTopVar s (LuaName v)
  | Just _ <- B.getEscaped v (s ^. B.topEscape) :: Maybe CoVar
  = True
isTopVar _ _ = False

wrapDriver :: MonadState ReplState m
           => StateT D.Driver (NameyT m) a
           -> m a
wrapDriver m = do
  state <- get
  ((res, drive), name)
    <- flip runNameyT (lastName state)
     . flip runStateT (driver state)
     $ m
  put state { lastName = name, driver = drive }
  pure res

wrapNamey :: MonadState ReplState m
          => NameyT m a
          -> m a
wrapNamey m = do
  (res, name) <- runNameyT m =<< gets lastName
  modify (\s -> s { lastName = name })
  pure res

emitCore :: MonadState ReplState m => [Stmt CoVar] -> m (LuaStmt, Bs.ByteString)
emitCore core = do
  emit <- gets emitState

  let core' = patchupUsage . snd . tagOccurStmt (const occursSet) OccursVar mempty $ core
      (luaStmt, emit') = uncurry B.addBuiltins $ runState (B.emitStmt core') emit
      luaExpr = LuaDo . map (patchupLua emit') . toList $ luaStmt
      luaSyntax = T.encodeUtf8 . display . uncommentDoc . renderPretty 0.8 100 . pretty $ luaExpr

  modify (\s -> s { emitState = emit' })
  pure (luaExpr, luaSyntax)

-- | Patchup the usage of a series of statements to ensure every one is
-- considered "used".
--
-- This guarantees we'll generate bindings for them all, and so they can
-- be evaluated in the REPL.
patchupUsage :: IsVar a => [C.AnnStmt b (OccursVar a)] -> [C.AnnStmt b(OccursVar a)]
patchupUsage [] = []
patchupUsage (s@C.Foreign{}:xs) = s:patchupUsage xs
patchupUsage (s@C.Type{}:xs) = s:patchupUsage xs
patchupUsage (s@C.RawCode{}:xs) = s:patchupUsage xs
patchupUsage (C.StmtLet (C.One v):xs) = C.StmtLet (C.One (first3 patchupVarUsage v)):patchupUsage xs
patchupUsage (C.StmtLet (C.Many v):xs) = C.StmtLet (C.Many (map (first3 patchupVarUsage) v)):patchupUsage xs

-- | Patchup the usage of a single variable. See 'patchupUsage' for more
-- information.
patchupVarUsage :: OccursVar a -> OccursVar a
patchupVarUsage (OccursVar v u) = OccursVar v (u <> Once)

completeInScope :: MonadState ReplState m => CompletionFunc m
completeInScope = completeWord Nothing " \n\t" $ \the_word -> do
  sig <- gets resolveScope
  let qual = T.splitOn (T.singleton '.') (T.pack the_word)
      prefix = init qual
      word = last qual
      prefix_str =
        case prefix of
          [] -> ""
          _ -> T.unpack (T.intercalate (T.singleton '.') prefix) ++ "."

  let comps = complete_from sig prefix word
  pure (map (\(Completion sub disp done) -> Completion (prefix_str ++ sub) disp done) comps)
  where
    complete_from (Signature vals tys mods) [] word =
      let sc = if not (T.null word)
                  then if isUpper (T.head word)
                          then (mod <$> mods) <> (val <$> vals)
                          else (val <$> vals) <> (val <$> tys)
                  else (mod <$> mods) <> (val <$> vals) <> (val <$> tys)
       in map snd $ filter (T.isPrefixOf word . fst) $ Map.toList sc
    complete_from (Signature _ _ our_mods) (m:mods) word =
      case Map.lookup m our_mods of
        Just (_, Just sig) -> complete_from sig mods word
        _ -> []

    val (SVar v) = simpleCompletion (T.unpack (nameName v))
    val (SAmbiguous (v:_)) = simpleCompletion (T.unpack (nameName v))
    val (SAmbiguous []) = undefined

    nameName (S.TgName v _) = v
    nameName (S.TgInternal v) = v

    mod (v, _) = Completion (T.unpack (nameName v `T.snoc` '.')) (T.unpack (nameName v)) False

-- | Reset the environment and load a series of files from environment
loadFile :: (MonadState ReplState m, MonadIO m) => Maybe FilePath -> m ()
loadFile file = withTimer ("Loading " ++ show file ++ " into the REPL") $ do
  -- Reset the state
  put =<< (liftIO . resetState) =<< get
  modify (\s ->  s { currentFile = file })
  wrapDriver D.tock

  -- Determine whether to load the prelude and an additional file.
  prelude <- gets (toList . prelude . config)
  load <- case file of
    Nothing -> pure []
    Just file -> do
      path <- liftIO $ canonicalizePath file
      exists <- liftIO $ doesFileExist path
      if exists
      then outputDoc ("Loading:" <+> verbatim file) $> [path]
      else outputDoc ("Cannot open" <+> verbatim file) $> []

  loadFiles (prelude ++ load) $> ()

loadFiles :: (MonadState ReplState m, MonadIO m) => [FilePath] -> m Bool
loadFiles [] = pure True
loadFiles paths = do
  (core, es) <- withTimer "REPL tick" $ wrapDriver $ D.tick >> D.compile paths

  files <- D.fileMap =<< gets driver
  handle <- gets outputHandle
  hReportAll handle files es
  case core of
    Nothing -> pure False
    Just core -> do
      for_ paths $ \path -> do
        (sig, env, lEnv) <- wrapDriver $ do
          ~(Just sig) <- D.getSignature path
          ~(Just env) <- D.getOpenedTypeEnv path
          ~(Just lEnv) <- D.getLowerState path
          pure (sig, env, lEnv)

        modify (\s -> s { resolveScope = resolveScope s <> sig
                        , inferScope = inferScope s <> env
                        , lowerState = lowerState s <> lEnv })

      lint <- gets (coreLint . config)
      when lint $ runLint "Lowered" (checkStmt emptyScope core) (pure ())

      (luaExpr, luaSyntax) <- emitCore core

      luaState <- gets luaState
      debug <- gets (debugMode . config)
      liftIO $ do
        dumpCore debug core core (pretty luaExpr)
        res <- L.runWith luaState $ do
          code <- L.dostring luaSyntax
          case code of
            L.OK -> pure (Right ())
            _ -> do
              val <- valueRepr (pure ())
              L.pop 1
              case val of
                String str -> pure (Left (text str <+> parens (shown code)))
                _ -> pure (Left (keyword "Error:" <+> pretty val))

        case res of
          Left err -> hPutDoc handle err >> pure False
          Right () -> pure True

outputDoc :: (MonadState ReplState m, MonadIO m) => Doc -> m ()
outputDoc x = do
  h <- gets outputHandle
  liftIO $ do
    hPutDoc h x
    hFlush h

parseRepl' :: Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed))
parseRepl' = first pure <$> parseRepl

parseCore :: (MonadState ReplState m, MonadIO m)
          => Parser (Either [S.Toplevel S.Parsed] (S.Expr S.Parsed))
          -> SourceName -> T.Text
          -> m (Maybe ([(CoVar, C.Type)]
                      , [S.Toplevel S.Typed]
                      , [Stmt CoVar]))
parseCore parser name input = do
  state <- get
  let files :: [(SourceName, T.Text)]
      files = [(name, input)]
      (parsed, parseMsg) = runParser name (L.fromStrict input) parser
  liftIO $ traverse_ (hReport (outputHandle state) files) parseMsg

  case parsed of
    Nothing -> pure Nothing
    Just parsed -> do
      let parsed' = case parsed of
            Left s -> s
            Right e -> [S.LetStmt S.NonRecursive S.Public [S.Binding (S.Name "_") e True (annotation e)]]

      (lower, es) <- wrapDriver $ do
        D.tick
        D.lowerWith (root (config state)) parsed' (resolveScope state) (inferScope state) (lowerState state)
      driver_files <- D.fileMap =<< gets driver
      hReportAll (outputHandle state) (files ++ driver_files) es
      case lower of
        Nothing -> pure Nothing
        Just (lower, lState, typed, env, ResolveResult _ _ sig) -> do
          let lastTerms = case lower of
                [] -> []
                _:_ -> case last lower of
                  (C.StmtLet (C.One (v, t, _))) -> [(v, t)]
                  (C.StmtLet (C.Many vs)) -> map (\(v, t, _) -> (v, t)) vs
                  _ -> []

          modify (\s -> s { resolveScope = sig, inferScope = env, lowerState = lState })
          pure $ Just (lastTerms, typed, lower)
