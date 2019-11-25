{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts,
   ScopedTypeVariables, CPP #-}

#ifdef WITH_SERVER
{-# LANGUAGE RecordWildCards #-}
#endif

module Main where

import System.Console.Haskeline hiding (display)
import System.Environment

import Control.Monad.IO.Class
import Control.Monad.Namey
import Control.Lens

import Parser.Wrapper (runParser)
import Parser (parseType, getL, Located)

import qualified Data.Text.Lazy as L

import Data.Text.Lazy (Text)
import Data.Foldable
import Data.Spanned
import Data.These
import Data.List

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Text.Pretty.Note (Note(..))
import Text.Pretty.Semantic

import Syntax.Transform
import Syntax.Builtin
import Syntax.Subst
import Syntax.Types
import Syntax

import Syntax.Resolve.Import (runNullImport)
import qualified Syntax.Resolve.Scope as R
import Syntax.Resolve
import Syntax.Desugar

import Types.Infer
import Types.Holes

import Frontend.Errors

import CompileTarget

import System.IO

#ifdef WITH_SERVER
import Control.Monad

import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import qualified Data.ByteString as Bs

import Network.HTTP.Toolkit as Http
import Network.Socket as Net

import System.Posix.Types
import System.Posix.Files
import System.Exit

import Control.Concurrent
import GHC.IO.Exception
#endif

main :: IO ()
main = do
  x <- getArgs
  case x of
    [] -> putDoc ("Welcome to" <+> keyword "amc-prove") *> runInputT defaultSettings prover
#ifdef WITH_SERVER
    ["serve", sock] -> startServer sock
#endif
    ("prove":x@(_:_)) -> handleSentence (pure ()) stdout (L.pack (unwords x))
    _ -> putDoc proverHelp

#ifdef WITH_SERVER
startServer :: String -> IO ()
startServer path = Net.withSocketsDo $ do
  unlink path

  sock <- Net.socket AF_UNIX Stream defaultProtocol

  flip catch (\e -> print (e :: SomeException) *> exitFailure) $ do
    Net.setSocketOption sock ReuseAddr 1
    Net.bind sock (SockAddrUnix path)
    Net.listen sock 4

  setFileMode path (CMode 0o666)

  bracket (pure ()) (\() -> unlink path) $ \() -> do
    forever $ do
      (sock, _) <- Net.accept sock
      _ <- forkIO $ handleSocketSentence (Net.socketToHandle sock ReadWriteMode)
      pure ()

  where
    handleSocketSentence geth = bracket geth hClose $ \handle -> do
      Http.Request{..} <- Http.readRequest False =<< Http.inputStreamFromHandle handle
      unless (requestMethod == "POST") $ do
        Http.simpleResponse (Bs.hPutStr handle) (toEnum 405) [] "Please use POST"
        exitFailure

      unless (("Host", "prove") `elem` requestHeaders) $ do
        Http.simpleResponse (Bs.hPutStr handle) (toEnum 404) [] "Not found: please request /prove"
        exitFailure

      body <- T.decodeUtf8 <$> Http.consumeBody requestBody

      tid <- forkIO $ do
        let success = do
              T.hPutStr handle "HTTP/1.1 200 OK\r\n"
              T.hPutStr handle "Content-Type: text/plain\r\n\r\n"
        handleSentence success handle . L.fromStrict $ body
        hClose handle
      threadDelay 3000000
      killThread tid
      o <- hIsOpen handle
      when o $ do
        T.hPutStr handle "HTTP/1.1 418 Request timed out\r\n\r\n"
        hClose handle

    unlink path =
      removeLink path
        `catch` \e ->
          case ioe_type e of
            NoSuchThing -> pure ()
            _ -> print e *> exitFailure
#endif

prover :: InputT IO ()
prover = do
  sentence <- getInputLine "? "
  case sentence of
    Nothing -> pure ()
    Just x | ":q" `isPrefixOf` x -> pure ()
    Just x | ":h" `isPrefixOf` x -> liftIO (putDoc proverHelp) *> prover
    Just t -> handleSentence (pure ()) stdout (L.pack t) *> prover

handleSentence :: MonadIO m => IO () -> Handle -> Text -> m ()
handleSentence success handle sentence =
  case runParser "<input>" sentence parseType of
    (Just t, _) -> flip evalNameyT (TgName "a" 0) $
      proveSentence (hReport handle files) success handle (fmap propVarToTv t)
    (Nothing, es) -> liftIO $ traverse_ (hReport handle files) es
  where files = [("<input>", L.toStrict sentence)]

propVarToTv :: Type Parsed -> Type Parsed
propVarToTv = transformType go where
  go :: Type Parsed -> Type Parsed
  go (TyPromotedCon v) = TyVar v
  go (TyLit v) = TyVar (Name (displayT (pretty v)))
  go x = x

proveSentence :: MonadIO m
              => (forall a. Note a Style => a -> IO ())
              -> IO ()
              -> Handle
              -> Located (Type Parsed) -> NameyT m ()
proveSentence report success stdout tau = do
  let prog = [ TySymDecl Public (Name "_") [] (foldr addForall t (ftv t)) (annotation tau) ]
      addForall v = TyForall v (Just TyType)
      t = getL tau
  x <- runNullImport $ resolveProgram lua rScope prog
  case x of
    Left es -> liftIO $ traverse_ report es
    Right (ResolveResult p _ _) -> do
      x <- inferProgram env =<< desugarProgram p
      case x of
        This es -> liftIO $ traverse_ report es
        These _ (p, _) -> solve p
        That (p, _) -> solve p
  where
    solve [ TypeDecl _ _ _ (Just [ArgCon _ _ ty _]) _ ] = do
      candidates <- findHoleCandidate mempty (annotation tau) env ty
      liftIO success
      if not (null candidates)
         then liftIO $ hPutDoc stdout (keyword "yes." <#> indent 2 (pretty (head candidates)))
         else liftIO $ hPutDoc stdout (keyword "probably not.")
    solve _ = undefined

rScope :: R.Signature
rScope = mempty { R._types = made } where
  made = foldr (\v m -> Map.insert (fst v) (R.SVar (snd v)) m) mempty builtins
  i x = (x, TgInternal x)
  builtins =
    Set.fromList [ i "+", i "not", i "ff", i "tt", i "<->", ("->", tyArrowName), ("*", tyTupleName) ]

env :: Env
env =
  mempty & types %~ mappend tys
         & names %~ focus (teleFromList bindings)
         & constructors %~ mappend cons
  where
    cons = Set.fromList [TgInternal "L", TgInternal "R", TgInternal "Not", TgInternal "T", TgInternal "Equiv"]
    tys = Map.fromList [ (TgInternal "+", Set.fromList [TgInternal "L", TgInternal "R"])
                       , (TgInternal "not", Set.fromList [TgInternal "Not"])
                       , (TgInternal "ff", mempty)
                       , (TgInternal "tt", Set.fromList [TgInternal "T"])
                       , (TgInternal "<->", Set.fromList [TgInternal "Equiv"])
                       ]

    bindings = [ (TgInternal "+", TyType :-> TyType :-> TyType)
               , (tyTupleName, TyType :-> TyType :-> TyType)
               , (tyArrowName, TyType :-> TyType :-> TyType)
               , ( TgInternal "L"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyVar a :-> TyOperator (TyVar a) (TgInternal "+") (TyVar b)
                 )
               , ( TgInternal "R"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyVar b :-> TyOperator (TyVar a) (TgInternal "+") (TyVar b)
                 )
               , ( TgInternal "<->", TyType :-> TyType :-> TyType )
               , ( TgInternal "Equiv"
                 , TyForall a (Just TyType) $
                   TyForall b (Just TyType) $
                     TyTuple (TyVar a :-> TyVar b)
                             (TyVar b :-> TyVar a)
                       :-> TyOperator (TyVar a) (TgInternal "<->") (TyVar b)
                 )
               , ( TgInternal "not", TyType :-> TyType )
               , ( TgInternal "Not"
                 , TyForall a (Just TyType) $
                   (TyVar a :-> TyForall b (Just TyType) (TyVar b)) :-> TyApps (TyCon (TgInternal "not")) [TyVar a]
                 )
               , (TgInternal "ff", TyType :-> TyType)
               , (TgInternal "tt", TyType :-> TyType)
               , ( TgInternal "T", TyCon (TgInternal "tt") )
               ]
    a = TgInternal "a"
    b = TgInternal "b"

proverHelp :: Doc
proverHelp = vsep
  [ "A prover for" <+> keyword "constructive quantifier-free first-order logic" <+> "based on amc's hole-filling."
  , empty
  , indent 2 "To prove a single proposition, run" <+> keyword "prove P"
#ifdef WITH_SERVER
  , indent 2 "Run with" <+> keyword "serve [path]" <+> "to open an HTTP server on the UNIX domain socket <path>"
  , indent 2 "If [path] exists, it will be unlinked. It will also be unlinked when the server stops."
#endif
  , empty
  , indent 2 . align $
      vsep [ bullet $ "Write propositional variables in" <+> stypeCon "UPPERCASE"
           , bullet $ "The logical connectives ∧ (AND) and ∨ (OR) are written infix as * and +"
              <#> indent 4 (hsep [ stypeCon "P" <+> soperator "+" <+> stypeCon "Q"
                                 , comma
                                 , stypeCon "P" <+> soperator "*" <+> stypeCon "Q"
                                 ])
           , bullet $ "The values ⊤ (TRUE) and ⊥ (FALSE) are spelled" <+> sliteral "tt" <+> "and" <+> sliteral "ff"
           , bullet $ "Negation is the function" <+> skeyword "not"
           , bullet $ "Additionally, propositional bi-implication may be written"
              <+> hsep [ stypeCon "P" <+> soperator "<->" <+> stypeCon "Q" ]
           ]
  , empty
  , keyword "Note:" <+> "amc-prove has difficulties dealing with uses of" <+> parens (soperator "+") <+> "inside" <+> parens (soperator "*")
  , indent 6 "please use multiple assumptions instead."
  , indent 6 "(Try"
      <+> hsep [ stypeCon "P" <+> arrow <+> stypeCon "Q" <+> arrow <+> stypeCon "R" ]
      <+> "instead of"
      <+> hsep [ stypeCon "P" <+> soperator "*" <+> stypeCon "Q" <+> arrow <+> stypeCon "R" ]
      <> ".)"
  ]
