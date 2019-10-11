{-# LANGUAGE
    DerivingStrategies
  , GeneralizedNewtypeDeriving
  , FlexibleContexts
  , UndecidableInstances
  , TemplateHaskell
  , ScopedTypeVariables
  , NamedFieldPuns
  , TupleSections
  , ViewPatterns
  , BlockArguments #-}

{-| The main frontend for compilation. This is responsible for loading
  files and programs, feeding them through the various stages of
  compilation, and caching the output.

  The aim of the driver is to be as lazy as possible - if we are resolving an
  expression and end up loading another module, then we don't need to type check
  the loaded module either.

-}
module Frontend.Driver
  ( Driver, emptyDriver
  , fileMap

  , resolve, resolveWith
  , infer, inferWith
  , lower, lowerWith
  , compile
  , getSignature, getTypeEnv, getVerified, getVerifiedAll
  ) where

import System.Directory
import System.FilePath

import Control.Monad.State.Strict
import Control.Monad.Namey
import Control.Applicative
import Control.Lens

import qualified Data.List.NonEmpty as E
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as L
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Bifunctor
import Data.Position
import Data.Foldable
import Data.Functor
import Data.Maybe
import Data.These
import Data.Span

import Core.Core (Stmt)
import Core.Var (CoVar)
import Core.Lower (LowerState, runLowerWithEnv, lowerProgEnv, defaultState)

import Types.Infer (inferProgram)

import Syntax.Resolve.Scope (Signature)
import Syntax.Resolve.Import
import Syntax.Resolve (ResolveResult(..), resolveProgram)
import Syntax.Builtin (builtinResolve, builtinEnv)
import Syntax.Desugar (desugarProgram)
import Syntax (Toplevel)
import Syntax.Verify
import Syntax.Types hiding (modules)
import qualified Syntax.Types as T
import Syntax.Var

import Parser.Wrapper (runParser)
import Parser (parseTops)

import Frontend.Errors

import Text.Pretty.Note

-- | The stage a module is at. Modules are parsed, resolved, type
-- checked, verified, and then lowered.
--
-- Modules may not go through all stages of a pipeline at once (or at all). The
-- only requirement is that their dependencies have been processed before they
-- have.
data Stage
  = SParsed     { _pBody :: [Toplevel Parsed] }
  | SUnparsed

  | SResolved   { _rBody :: [Toplevel Resolved], sig :: Signature }
  | SUnresolved

  | STyped      { _tBody :: [Toplevel Typed],    sig :: Signature, env :: Env }
  | SUntyped    {                                sig :: Signature             }

  | SVerified   { _tBody :: [Toplevel Typed],    sig :: Signature, env :: Env }
  | SUnverified {                                sig :: Signature, env :: Env }

  | SEmitted    { _cBody :: [Stmt CoVar],        sig :: Signature, env :: Env, _lowerState :: LowerState }

data Module = Module
  { modLocation :: FilePath
  , modSource :: SourceName
  , modVar :: Name

  -- | Modules which depend on this one.
  , _dependencies :: Set.Set FilePath
  -- | The first module which depends on this one. Used for producing
  -- traces for cyclic dependencies.
  , _dependent :: Maybe (FilePath, Span)

  -- | The stage through the pipeline of this module.
  , _stage :: Stage

  , _errors :: ErrorBundle
  }

data Driver = Driver
  { -- | All loaded modules
    _modules :: Map.Map FilePath Module
  -- | A mapping of pretty source paths, to cannonical file names.
  , _filePaths :: Map.Map SourceName FilePath
  }

makeLenses ''Module
makeLenses ''Driver

emptyDriver :: Driver
emptyDriver = Driver mempty mempty

fileMap :: MonadIO m => Driver -> m FileMap
fileMap driver
  = liftIO
  . traverse (\mod -> (modSource mod,) <$> T.readFile (modLocation mod))
  . Map.elems
  $ driver ^. modules

-- | Resolve a term with the current driver.
resolve :: (MonadNamey m, MonadIO m, MonadState Driver m)
        => FilePath -> [Toplevel Parsed]
        -> m (Maybe ResolveResult, ErrorBundle)
resolve root parsed = resolveWith root parsed builtinResolve

-- | Resolve a term with the current driver and a custom environment.
resolveWith :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed] -> Signature
            -> m (Maybe ResolveResult, ErrorBundle)
resolveWith root parsed sig = fst <$> resolveWithDeps root parsed sig

resolveWithDeps :: (MonadNamey m, MonadIO m, MonadState Driver m)
                => FilePath -> [Toplevel Parsed] -> Signature
                -> m ((Maybe ResolveResult, ErrorBundle), Set.Set FilePath)
resolveWithDeps root parsed sig = do
  (resolved, deps) <- flip runFileImport (LoadContext root Nothing)
                 $ resolveProgram sig parsed
  (,deps) <$> case resolved of
    Left es -> pure (Nothing, mempty & (resolveErrors .~ es)) -- TODO: Locate errors
    Right resolved -> pure (Just resolved, mempty)

-- | Infer a term with the current driver.
infer :: (MonadNamey m, MonadIO m, MonadState Driver m)
      => FilePath -> [Toplevel Parsed]
      -> m ( Maybe ([Toplevel Typed], Env, ResolveResult)
           , ErrorBundle)
infer root parsed = inferWith root parsed builtinResolve builtinEnv

-- | Infer a term with the current driver and a custom environment.
inferWith :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed] -> Signature -> Env
            -> m ( Maybe ([Toplevel Typed], Env, ResolveResult)
                 , ErrorBundle)
inferWith root parsed sig env = fst <$> inferWithDeps root parsed sig env

inferWithDeps :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed] -> Signature -> Env
            -> m (( Maybe ([Toplevel Typed], Env, ResolveResult)
                  , ErrorBundle), Set.Set FilePath)
inferWithDeps root parsed sig env = do
  ((res, errs), deps) <- resolveWithDeps root parsed sig
  (,deps) <$> case res of
    Nothing -> pure (Nothing, errs)
    Just r@(ResolveResult resolved _ _) -> do
      (env, errs) <- second (<>errs) <$> checkAll getTypeEnv env deps
      case env of
        Nothing -> pure (Nothing, errs)
        Just env -> do
          inferred <- inferProgram env =<< desugarProgram resolved
          pure $ case inferred of
            That (tBody, env') -> (Just (tBody, env', r), errs)
            This es -> (Nothing, errs & typeErrors .~ es)
            These es _ | any isError es -> (Nothing, errs & typeErrors .~ es)
            These es (tBody, env') -> (Just (tBody, env', r), errs & typeErrors .~ es)

-- | Lower a term with the current driver
lower :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed]
            -> m ( Maybe ([Stmt CoVar], LowerState, [Toplevel Typed], Env, ResolveResult)
                 , ErrorBundle)
lower root parsed = lowerWith root parsed builtinResolve builtinEnv defaultState

-- | Lower a term with the current driver and a custom environment.
lowerWith :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed] -> Signature -> Env -> LowerState
            -> m ( Maybe ([Stmt CoVar], LowerState, [Toplevel Typed], Env, ResolveResult)
                 , ErrorBundle)
lowerWith root parsed sig env lState = do
  ((res, errs), deps) <- inferWithDeps root parsed sig env
  case res of
    Nothing -> pure (Nothing, errs)
    Just (inferred, env, resolved) -> do
      (verified, errs) <- second (errs<>) <$> checkAll getVerifiedAll () deps
      v <- genName
      let (verified', errs') = verifyProg v env inferred

      (,errs<>errs') <$> case (verified, verified') of
        (Nothing, _) -> pure Nothing
        (_, False) -> pure Nothing

        (Just (), True) -> do
          (_, lEnv, ls) <- lowerIts False deps mempty
          (lEnv, l) <- runLowerWithEnv (lState <> lEnv) (lowerProgEnv inferred)
          pure (Just (concat (ls Seq.|> l), lEnv, inferred, env, resolved))

-- | Attempt to compile a single file.
compile :: (MonadNamey m, MonadIO m, MonadState Driver m)
        => FilePath -> m (Maybe [Stmt CoVar], ErrorBundle)
compile path = do
  (ok, errs) <- getVerifiedAll path
  case ok of
    Nothing -> pure (Nothing, errs)
    Just () -> do
      (_, _, lowered) <- lowerIt True path mempty
      pure (Just (concat lowered), errs)

lowerIt :: (MonadNamey m, MonadState Driver m)
        => Bool -> FilePath
        -> (Set.Set FilePath, LowerState, Seq.Seq [Stmt CoVar])
        -> m (Set.Set FilePath, LowerState, Seq.Seq [Stmt CoVar])
lowerIt all path (visited, lEnv, stmts)
  | path `Set.member` visited = pure (visited, lEnv, stmts)
  | otherwise = do
      mod <- findMod path
      (lEnv, stmts) <- case mod ^. stage of
        SEmitted l _ _ lEnv' -> pure (lEnv <> lEnv', if all then stmts Seq.|> l else stmts)
        SVerified prog sig env -> do
          (lEnv, l) <- runLowerWithEnv lEnv (lowerProgEnv prog)
          updateMod path $ stage .~ SEmitted l sig env lEnv
          pure (lEnv, stmts Seq.|> l)
        _ -> error "Impossible: Should have been verified"
      pure (Set.insert path visited, lEnv, stmts)

lowerIts :: (MonadNamey m, MonadState Driver m)
         => Bool -> Set.Set FilePath
         -> (Set.Set FilePath, LowerState, Seq.Seq [Stmt CoVar])
         -> m (Set.Set FilePath, LowerState, Seq.Seq [Stmt CoVar])
lowerIts all paths state = foldrM (lowerIt all) state paths

addModule :: (MonadNamey m, MonadState Driver m, MonadIO m)
          => FilePath -> m Module
addModule path = do
  name <- liftIO $ makeRelativeToCurrentDirectory path
  var <- genNameFrom (T.pack ("\"" ++ name ++ "\""))
  contents <- liftIO $ L.readFile path
  let (parsed, es) = runParser name contents parseTops
  let mod = Module
        { modLocation = path
        , modSource = name
        , modVar = var

        , _dependencies = mempty
        , _dependent = Nothing

        , _stage = maybe SUnparsed SParsed parsed

        , _errors = mempty & parseErrors .~ es
        }

  filePaths %= Map.insert name path
  modules %= Map.insert path mod

  pure mod

-- | Get or compute a module's signature.
--
-- This returns the signature, if it could be computed successfully, as
-- well any errors encountered when satisfying the request.
getSignature :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => FilePath -> m (Maybe Signature, ErrorBundle)
getSignature path = do
  mod <- uses modules (Map.lookup path)
  mod <- case mod of
    Nothing -> addModule path
    Just mod -> pure mod

  case mod ^. stage of
    SUnparsed -> pure (Nothing, mod ^. errors)
    SParsed parsed -> do
      (resolved, deps) <-
          flip runFileImport (LoadContext (dropFileName path) (Just path))
        $ resolveProgram builtinResolve parsed
      case resolved of
        Left es -> do
          updateMod path $ (stage .~ SUnresolved) . (dependencies .~ deps) . (errors . resolveErrors .~ es)
          (Nothing,) . (^.errors) <$> findMod path
        Right (ResolveResult resolved sig _) -> do
          updateMod path $ (stage .~ SResolved resolved sig) . (dependencies .~ deps)
          (Just sig,) . (^.errors) <$> findMod path

    SUnresolved -> pure (Nothing, mempty)
    stage -> pure (Just (sig stage), mempty)

-- | Get or compute a module's type environment.
--
-- This returns the environment, if it could be computed successfully, as
-- well as an errors encountered while satisfying the request (such as
-- type errors in imported modules, etc...)
getTypeEnv :: (MonadNamey m, MonadState Driver m, MonadIO m)
            => FilePath -> m (Maybe Env, ErrorBundle)
getTypeEnv path = do
  (_, depErrors) <- getSignature path

  mod <- findMod path
  case mod ^. stage of
    SParsed{} -> pure (Nothing, depErrors)
    SUntyped{} -> pure (Nothing, depErrors)
    SUnresolved{} -> pure (Nothing, depErrors)

    SResolved { _rBody = rBody, sig } -> do
      (env, depErrors) <- second (depErrors<>) <$> checkAll getTypeEnv builtinEnv (mod ^. dependencies)
      case env of
        -- One of our dependencies failed: Mark us as failed too and abort.
        Nothing -> updateMod path (stage .~ SUntyped sig) $> (Nothing, depErrors)
        Just env -> do
          inferred <- inferProgram env =<< desugarProgram rBody
          let (res, es) = case inferred of
                That res -> (Just res, [])
                This es -> (Nothing, es)
                These es _ | any isError es -> (Nothing, es)
                These es res -> (Just res, es)

          case res of
            Nothing -> do
              updateMod path $ (stage .~ SUntyped sig) . (errors . typeErrors .~ es)
              (Nothing,) . (depErrors<>) . (^.errors) <$> findMod path
            Just (tBody, modEnv) ->
              let env' = env
                    & (names %~ (<> (modEnv ^. names)))
                    . (types %~ (<> (modEnv ^. types)))
                    . (classDecs %~ (<> (modEnv ^. classDecs)))
                    . (T.modules %~ (<> (modEnv ^. T.modules))
                        . Map.insert (modVar mod) (modEnv ^. classes, modEnv ^. tySyms))
              in do updateMod path $ (stage .~ STyped tBody sig env') . (errors . typeErrors .~ es)
                    (Just env',) . (depErrors<>) . (^.errors) <$> findMod path

    stage -> pure (Just (env stage), depErrors)

-- | Determine whether a module can be successfully verified.
--
-- The error handle includes all errors resulting from /this verification
-- pass/.
getVerified :: (MonadNamey m, MonadState Driver m, MonadIO m)
             => FilePath -> m (Maybe (), ErrorBundle)
getVerified path = do
  (_, depErrors) <- getTypeEnv path

  mod <- findMod path
  case mod ^. stage of
    SParsed{} -> pure (Nothing, mempty)
    SUnparsed{} -> pure (Nothing, mempty)
    SUntyped{} -> pure (Nothing, mempty)
    SUnresolved{} -> pure (Nothing, mempty)

    SResolved{} -> error "Impossible resolved - should have been typed."

    -- Already done
    SUnverified{} -> pure (Nothing, mempty)
    SVerified{} -> pure (Just (), mempty)
    SEmitted{} -> pure (Just (), mempty)

    STyped prog sig env -> do
      v <- genName
      let (verified, errs) = verifyProg v env prog
      updateMod path $ (stage .~ if verified then SVerified prog sig env else SUnverified sig env)
                     . (errors %~ (<>errs))
      pure ( if verified then Just () else Nothing
           , depErrors <> errs)

getVerifiedAll :: (MonadNamey m, MonadState Driver m, MonadIO m)
             => FilePath -> m (Maybe (), ErrorBundle)
getVerifiedAll path = do
  (here, errors) <- getVerified path

  mod <- findMod path
  (there, depErrors) <- checkAll getVerifiedAll () (mod ^. dependencies)

  pure (here >> there, depErrors <> errors)

verifyProg :: Name -> Env -> [Toplevel Typed] -> (Bool, ErrorBundle)
verifyProg v env inferred =
  let verified' = runVerify env v (verifyProgram inferred)
  in case verified' of
    Right () -> (True, mempty)
    Left es -> (any isError es, mempty & verifyErrors .~ toList es)

-- | Update an item within the state
updateMod :: MonadState Driver m
          => FilePath -> (Module -> Module) -> m ()
updateMod path f = modules %= Map.update (Just . f) path


{- $importer

   Modules are originally located and inserted into the cache when
   running resolution. The process is relatively simple:

    - 'importFile': This attempts to load a file from the cache. If the
      file has not yet been loaded, then it will load it from disk
      instead.and pass it to the resolver. If the file is already being
      resolved when imported, then we determine that there is a cyclic
      import and error.

    - 'FileImport'/'runFileImport': Resolution is run within this
      monad. This is effectively 'importFile' specialised for a module -
      it supports relative paths, and tracks dependencies between
      modules.
-}

data LoadContext = LoadContext
  { _curDir :: FilePath
  -- ^ The path to lookup files relative to
  , _source :: Maybe FilePath
  -- ^ The current file name. Used for determining blame in cyclic
  -- dependencies.
  }

newtype FileImport m a = FileIm
  { runFileImport :: LoadContext -> m (a, Set.Set FilePath) }

instance Functor f => Functor (FileImport f) where
  fmap f (FileIm go) = FileIm \c -> ((\(a, w) -> (f a, w)) <$> go c)

instance Applicative f => Applicative (FileImport f) where
  pure x = FileIm (\_ -> pure (x, mempty))
  (FileIm f) <*> (FileIm x) = FileIm \c -> liftA2 k (f c) (x c)
    where k (f, w) (x, w') = (f x, w <> w')

instance Monad m => Monad (FileImport m) where
  x >>= f = FileIm \c -> do
    (x, w) <- runFileImport x c
    (y, w') <- runFileImport (f x) c
    pure (y, w <> w')

instance MonadTrans FileImport where
  lift m = FileIm \_ -> (,mempty) <$> m

instance MonadNamey m => MonadNamey (FileImport m) where
  genName = lift genName

instance (MonadNamey m, MonadState Driver m, MonadIO m)
      => MonadImport (FileImport m) where
  importModule loc relPath = FileIm \(LoadContext curDir source) -> do
    absPath <- liftIO $ canonicalizePath (curDir </> T.unpack relPath)
    (,Set.singleton absPath) <$> importFile source (Just loc) absPath

-- | Import a file from the current directory.
importFile :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => Maybe FilePath -> Maybe Span -> FilePath -> m ImportResult
importFile fromPath fromLoc path = do
  mod <- uses modules (Map.lookup path)
  case mod of
    Just mod ->
      case mod ^. stage of
        SUnresolved -> pure Errored
        SUnparsed -> pure Errored
        SParsed _ -> do
          let ~(Just loc) = fromLoc
          state <- get
          pure (ImportCycle ((modSource mod, loc) E.:| findCycle mod state))

        stage -> pure (Imported (modVar mod) (sig stage))

    Nothing -> do
      exists <- liftIO $ doesFileExist path
      if not exists then pure NotFound else do
        mod <- addModule path
        updateMod path $ dependent .~ ((,) <$> fromPath <*> fromLoc)
        maybe Errored (Imported (modVar mod)) . fst <$> getSignature path

-- | Try to identify the cycle of modules requiring each other.
findCycle :: Module -> Driver -> [(FilePath, Span)]
findCycle (Module { modSource, _stage = SParsed _, _dependent = Just (from, loc) }) st =
  (modSource, loc) : findCycle (fromJust (Map.lookup from (st ^. modules))) st
findCycle _ _ = []

newtype AllOf a = AllOf (Maybe a)

instance Semigroup a => Semigroup (AllOf a) where
  (AllOf (Just x)) <> (AllOf (Just y)) = AllOf (Just (x <> y))
  _ <> _ = AllOf Nothing

-- | Akin to 'sequenceA', this maps over a collection, accumulating the
-- results only if all functions return 'Just'.
checkAll :: (Monad m, Foldable t, Semigroup b)
         => (a -> m (Maybe b, ErrorBundle)) -> b -> t a -> m (Maybe b, ErrorBundle)
checkAll fn start xs
    = first (\(AllOf x) -> x)
  <$> foldrM (\x acc -> (<>acc) . first AllOf <$> fn x)
             (AllOf (Just start), mempty) xs

-- | Find a module within the state, knowing that it is present.
findMod :: MonadState Driver m => FilePath -> m Module
findMod p = uses modules (fromJust . Map.lookup p)

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage
