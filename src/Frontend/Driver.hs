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

  The aim of the driver is to be as lazy as possible - if we are
  resolving an expression and end up loading another file, then we don't
  need to type check the loaded file either.

-}
module Frontend.Driver
  ( Driver
  , makeDriver, makeDriverWith
  , DriverConfig(..), makeConfig
  , DriverCallbacks(..), defaultCallbacks
  , fileMap
  , getConfig, adjustConfig

  -- * Cache invalidation
  , tick, tock

  -- * REPL interaction
  --
  -- $repl
  , resolve, resolveWith
  , infer, inferWith
  , lower, lowerWith
  , locatePrelude

  -- * Compilation
  --
  -- $compile
  , compile, compiles

  -- * Querying the driver
  --
  -- $query
  , getSignature, getTypeEnv, getOpenedTypeEnv
  , getVerified, getVerifiedAll
  , getLowerState, getLowered
  , getErrors, getErrorsAll
  ) where

import System.Environment
import System.Directory
import System.FilePath

import Control.Monad.State.Strict
import Control.Monad.Namey
import Control.Applicative
import Control.Timing
import Control.Lens hiding ((<.>))

import qualified Data.Text.Lazy.Encoding as L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.List.NonEmpty as E
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Position
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Monoid
import Data.Maybe
import Data.These
import Data.Span

import qualified Crypto.Hash.SHA256 as SHA

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
import Syntax.Types
import Syntax.Var

import Parser.Wrapper (runParser)
import Parser (parseTops)

import Frontend.Errors

import Text.Pretty.Note

-- | The stage a file is at. Files are parsed, resolved, type checked,
-- verified, and then lowered.
--
-- Files may not go through all stages of a pipeline at once (or at
-- all). The only requirement is that their dependencies have been
-- processed before they have.
--
-- Note, while 'sig' represents the current file's module signature,
-- 'env' and '_lowerState' are the /accumulated/ state of this file and
-- all it dependencies.
data Stage
  = SParsed     { _pBody :: [Toplevel Parsed] }
  | SUnparsed

  | SResolving
  | SResolved   { _rBody :: [Toplevel Resolved], sig :: Signature }
  | SUnresolved

  | STyped      { _tBody :: [Toplevel Typed],    sig :: Signature, env :: Env }
  | SUntyped    {                                sig :: Signature             }

  | SVerified   { _tBody :: [Toplevel Typed],    sig :: Signature, env :: Env }
  | SUnverified {                                sig :: Signature, env :: Env }

  | SEmitted    { _cBody :: [Stmt CoVar],        sig :: Signature, env :: Env, _lowerState :: LowerState }
  deriving Show

-- | A clock, composed of a major 'cTock' counter and a minor 'cTick' counter.
--
-- Ticks represent a "minor" change to the cache, which does not require
-- us to update the executable state. Any file which has not yet been
-- emitted may be reloaded at this point.
--
-- Tocks are used for a major change, where we will dispose of the whole
-- executable state. Any module may be reloaded at this point.
data Clock = Clock { cTock :: Int, cTick :: Int }
  deriving (Show, Eq, Ord)

data LoadedFile = LoadedFile
  { fileLocation :: FilePath
  , fileSource   :: SourceName
  , fileVar      :: Name

  , fileHash        :: BS.ByteString -- ^ A SHA256 hash of this file's contents

  -- | The time this file was last loaded. This must be greater or equal
  -- to any of its dependencies.
  , fileLoadClock   :: Clock
  -- | The time this file was last checked for changes. This is used as a
  -- mechanism to prevent checking every time.
  , _fileCheckClock :: Clock

  -- | Files upon which this one depends.
  , _dependencies :: Set.Set FilePath
  -- | The first module upon which this one depends. Used for producing
  -- traces for cyclic dependencies.
  , _dependent :: Maybe (FilePath, Span)

  -- | The stage through the pipeline of this file.
  , _stage :: Stage

  , _errors :: ErrorBundle
  } deriving Show

data DriverCallbacks = DriverCallbacks
  { onResolved :: FilePath -> Maybe ([Toplevel Resolved], Signature) -> ErrorBundle -> IO ()
  -- | Provides the program, the environment, and the starting
  -- environment (suitable for identifying new names).
  , onTyped    :: FilePath -> Maybe ([Toplevel Typed], Env, Env) -> ErrorBundle -> IO ()
  , onVerified :: FilePath -> Maybe ([Toplevel Typed], Env) -> ErrorBundle -> IO ()
  , onEmitted  :: FilePath -> [Stmt CoVar] -> IO ()
  }

defaultCallbacks :: DriverCallbacks
defaultCallbacks = DriverCallbacks f f f (\_ _ -> pure ()) where
  f :: FilePath -> Maybe a -> ErrorBundle -> IO ()
  f _ _ _ = pure ()

instance Show DriverCallbacks where
  show _ = "DriverCallbacks"

data DriverConfig = DriverConfig
  { libraryPath :: [FilePath]
  -- ^ The path of folders to look up files from.
  , callbacks :: DriverCallbacks
  -- ^ Callbacks for when a module has finished a particular stage.
  , checkOnly :: Bool
  } deriving Show

data Driver = Driver
  { -- | All loaded files.
    _files :: Map.Map FilePath LoadedFile
    -- | The current clock of the loader.
  , _clock :: Clock
  -- | The driver's current config
  , _config :: DriverConfig
  } deriving Show

makeLenses ''LoadedFile
makeLenses ''Driver

-- | Construct a new driver from the given config.
makeDriverWith :: DriverConfig -> Driver
makeDriverWith = Driver mempty (Clock 0 0)

-- | Construct a new driver using 'makeConfig'.
makeDriver :: IO Driver
makeDriver = Driver mempty (Clock 0 0) <$> makeConfig

-- | The default driver config.
--
-- This constructs a library path which will attempt to locate a file
-- from one of the "known directories" - $AMC_LIBRARY_PATH, ../lib/
-- relative to the compiler's executable, or lib/ relative to the
-- compiler's executable.
makeConfig :: IO DriverConfig
makeConfig = do
  execP <- getExecutablePath
  mainPath <- foldMap splitPath <$> lookupEnv "AMC_LIBRARY_PATH"

  pure $ DriverConfig
    (mainPath
    ++ [ takeDirectory execP </> "lib"
       , takeDirectory (takeDirectory execP) </> "lib"
       ])
    defaultCallbacks
    False
  where
    splitPath = Set.toList . Set.fromList . map T.unpack . T.split (==':') . T.pack

-- | Locate the "prelude" module from a set of known locations:
-- * The AMC_PRELUDE environment variable (a file)
-- * $libraryPath/prelude.ml
locatePrelude :: MonadIO m => DriverConfig -> m (Maybe FilePath)
locatePrelude config = withTimer "Locating prelude" do
  let libPath = map (</> "prelude.ml") (libraryPath config)
  liftIO $ findFile' =<< (++) <$> (toList <$> lookupEnv "AMC_PRELUDE") <*> pure libPath

-- | Construct a file map, suitable for use with 'fileSpans' and other
-- "Text.Pretty.Note" functions.
fileMap :: MonadIO m => Driver -> m FileMap
fileMap driver
  = liftIO
  . traverse (\file -> (fileSource file,) <$> T.readFile (fileLocation file))
  . Map.elems
  $ driver ^. files

-- | Get the current config
getConfig :: Driver -> DriverConfig
getConfig = _config

adjustConfig :: MonadState Driver m => (DriverConfig -> DriverConfig) -> m ()
adjustConfig = (config%=)

-- | Update the drivers's internal counter, allowing to reload any
-- non-emitted files.
tick :: MonadState Driver m => m ()
tick = clock %= \(Clock to ti) -> Clock to (ti + 1)

-- | Update the driver's internal counter, allowing it to reload any
-- emitted file.
--
-- This should only be used in conjunction with resetting your execution
-- state (such as a Lua environment).
tock :: MonadState Driver m => m ()
tock = clock %= \(Clock to _) -> Clock (to + 1) 0

{- $repl

   These functions provide a way of loading an external expression,
   utilising the driver when loading external files.
-}

-- | Resolve a term with the current driver.
resolve :: (MonadNamey m, MonadIO m, MonadState Driver m)
        => FilePath -> [Toplevel Parsed]
        -> m (Maybe ResolveResult, ErrorBundle)
resolve root parsed = resolveWith root parsed builtinResolve

-- | Resolve a term with the current driver and a custom environment.
resolveWith :: (MonadNamey m, MonadIO m, MonadState Driver m)
            => FilePath -> [Toplevel Parsed] -> Signature
            -> m (Maybe ResolveResult, ErrorBundle)
resolveWith root parsed sig = errorsFromDeps =<< resolveWithDeps root parsed sig

resolveWithDeps :: (MonadNamey m, MonadIO m, MonadState Driver m)
                => FilePath -> [Toplevel Parsed] -> Signature
                -> m ((Maybe ResolveResult, ErrorBundle), Set.Set FilePath)
resolveWithDeps root parsed sig = withTimer ("Resolving " ++ root) do
  (resolved, deps) <- flip runFileImport (LoadContext root Nothing)
                    $ resolveProgram sig parsed
  (,deps) <$> case resolved of
    Left es -> pure (Nothing, mempty & (resolveErrors .~ es))
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
inferWith root parsed sig env = errorsFromDeps =<< inferWithDeps root parsed sig env

inferWithDeps :: (MonadNamey m, MonadIO m, MonadState Driver m)
              => FilePath -> [Toplevel Parsed] -> Signature -> Env
              -> m (( Maybe ([Toplevel Typed], Env, ResolveResult)
                    , ErrorBundle), Set.Set FilePath)
inferWithDeps root parsed sig env = do
  ((res, errors), deps) <- resolveWithDeps root parsed sig
  (,deps) <$> case res of
    Nothing -> pure (Nothing, errors)
    Just r@(ResolveResult resolved _ _) -> withTimer ("Type checking " ++ root) do
      AllOf env' <- foldMapM (fmap AllOf . getTypeEnv) deps
      case env' of
        Nothing -> pure (Nothing, errors)
        Just env' -> do
          inferred <- inferProgram (env <> env') =<< desugarProgram resolved
          pure $ case inferred of
            That (tBody, env') -> (Just (tBody, env', r), errors)
            This es -> (Nothing, errors & typeErrors .~ es)
            These es _ | any isError es -> (Nothing, errors & typeErrors .~ es)
            These es (tBody, env') -> (Just (tBody, env', r), errors & typeErrors .~ es)

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
  ((res, errors), deps) <- inferWithDeps root parsed sig env
  c <- use config
  case res of
    Nothing -> (Nothing, ) . (errors<>) . fold <$> gatherDepsOf getErrors deps

    Just (inferred, env, resolved) | checkOnly c -> withTimer ("Lowering " ++ root) do
      pure (Just ([], mempty, inferred, env, resolved), errors)

    Just (inferred, env, resolved) -> do
      All verified <- foldMapM getVerifiedAll deps
      errors <- fold <$> gatherDepsOf getErrors deps

      v <- genName
      let (verified', errs') = verifyProg v env inferred

      (,errors<>errs') <$> case (verified, verified') of
        (False, _) -> pure Nothing
        (_, False) -> pure Nothing

        (True, True) -> do
          ~(Just (ls, lEnv)) <- fmap Seq.unzip . sequence <$> gatherDepsOf getNewlyLowered deps
          (lEnv, l) <- runLowerWithEnv (lState <> fold lEnv) (lowerProgEnv inferred)
          pure (Just (concat (ls Seq.|> l), lEnv, inferred, env, resolved))

  where
    getNewlyLowered path = do
      stg <- uses (files . at path) (fmap (^.stage))
      case stg of
        Just (SEmitted _ _ _ lEnv) -> pure (Just ([], lEnv))
        _ -> getLowered path

{- $compile

   Various helper functions for compiling a whole bundle of files.
 -}

-- | Attempt to compile a single of file. Returns the concatenated core
-- of all files.
compiles :: (MonadNamey m, MonadIO m, MonadState Driver m)
         => FilePath -> m (Maybe [Stmt CoVar], ErrorBundle)
compiles = compile . pure

-- | Attempt to compile a collection of files. Returns the concatenated
-- core of all files.
compile :: (MonadNamey m, MonadIO m, MonadState Driver m)
        => [FilePath] -> m (Maybe [Stmt CoVar], ErrorBundle)
compile ps = do
  let paths = Set.fromList ps
  l <- fmap (concat . fmap fst) . sequence <$> gatherDepsOf getLowered paths
  errors <- fold <$> gatherDepsOf getErrors paths
  pure (l, errors)

errorsFromDeps :: (MonadNamey m, MonadState Driver m)
               => ((Maybe a, ErrorBundle), Set.Set FilePath)
               -> m (Maybe a, ErrorBundle)
errorsFromDeps ((result, errors), deps) = do
  errors' <- fold <$> gatherDepsOf getErrors deps
  pure (result, errors' <> errors)

gatherDepsOf :: (MonadNamey m, MonadState Driver m)
             => (FilePath -> m a)
             -> Set.Set FilePath -> m (Seq.Seq a)
gatherDepsOf f = fmap snd . foldlM go mempty where
  go (visited, seq) path
    | path `Set.member` visited = pure (visited, seq)
    | otherwise = do
        this <- f path
        deps <- uses (files . at path) (foldMap (^.dependencies))
        (visited, seq) <- foldlM go (Set.insert path visited, seq) deps
        pure (visited, seq Seq.|> this)

verifyProg :: Name -> Env -> [Toplevel Typed] -> (Bool, ErrorBundle)
verifyProg v env inferred =
  let (ok, es) = runVerify env v (verifyProgram inferred)
  in (ok, mempty & verifyErrors .~ toList es)

{- $query

   We provide various methods for querying "the oracle". These take some
   /absolute/ file path, and perform whatever compilation steps required
   in order to produce the required output.

   All functions also return an 'ErrorBundle', holding any errors which
   occurred in the process of loading this file.
-}

-- | Get a file, reloading from disk if the cache state has changed.
getFile :: forall m. (MonadNamey m, MonadState Driver m, MonadIO m)
        => FilePath -> m (Maybe LoadedFile)
getFile = reloadFile where
  -- | Get or reload a file, returning it.
  --
  -- Note, the file's check clock should be its previous one in the event
  -- the file did not change. The file within the Driver state will be
  -- updated to have the latest time.
  reloadFile :: FilePath -> m (Maybe LoadedFile)
  reloadFile path = do
    file <- use (files . at path)
    clock <- use clock
    case file of
      Nothing -> do
        contents <- read path
        case contents of
          Nothing -> pure Nothing
          Just (sha, contents) -> do
            -- File isn't in cache: add it.
            name <- liftIO $ makeRelativeToCurrentDirectory path
            var <- genNameFrom (T.pack ("\"" ++ name ++ "\""))
            Just <$> addFile path (T.pack name) var sha contents

      Just file
        -- We've already checked this tick, don't do anything.
        | file ^. fileCheckClock == clock -> pure (Just file)

        | SEmitted{} <- file ^. stage
        , cTock (file ^. fileCheckClock) == cTock clock ->
            -- If we've emitted the file, and we're on the same major tick, then
            -- it's not safe to recompile - we don't want to break any REPL
            -- state. So just update the clock.
            updateFile path (fileCheckClock .~ clock) $> (Just file)

        | otherwise -> do
          contents <- read path
          case contents of
            Nothing ->
              -- Remove file from cache if it doesn't exist on disk
              (files %= Map.delete path) $> Nothing
            Just (sha, contents)
              | sha /= fileHash file ->
                -- If it's been updated on disk, just reload it immediately.
                Just <$> addFile path (fileSource file) (fileVar file) sha contents
              | otherwise -> do
                -- Otherwise check each dependency. We update the clock beforehand, to
                -- avoid getting into any dependency loops.
                updateFile path (fileCheckClock .~ clock)
                -- If this file has been loaded before its dependency, then the dependency
                -- was loaded on a later clock tick, and so we're out of date.
                Any changed <- foldMapM (fmap (Any . maybe True (on (<) fileLoadClock file)) . reloadFile) (file ^. dependencies)
                if changed
                then Just <$> addFile path (fileSource file) (fileVar file) sha contents
                else pure (Just file)

  read :: FilePath -> m (Maybe (BS.ByteString, BSL.ByteString))
  read path = do
    exists <- liftIO $ doesFileExist path
    if not exists then pure Nothing else do
      contents <- liftIO $ BSL.readFile path
      pure (Just (SHA.hashlazy contents, contents))

  addFile :: FilePath -> SourceName -> Name -> BS.ByteString -> BSL.ByteString -> m LoadedFile
  addFile path source var hash contents = do
    clock <- use clock
    let (parsed, es) = runParser source (L.decodeUtf8 contents) parseTops
    let file = LoadedFile
          { fileLocation = path
          , fileSource   = source
          , fileVar      = var

          , fileHash        = hash
          , fileLoadClock   = clock
          , _fileCheckClock = clock

          , _dependencies = mempty
          , _dependent = Nothing

          , _stage = maybe SUnparsed SParsed parsed

          , _errors = mempty & parseErrors .~ es
          }

    files %= Map.insert path file

    pure file

-- | Get or compute a file's signature.
getSignature :: (MonadNamey m, MonadState Driver m, MonadIO m)
             => FilePath -> m (Maybe Signature)
getSignature path = do
  file <- fromMaybe (error ("Cannot find " ++ show path)) <$> getFile path
  case file ^. stage of
    SUnparsed -> pure Nothing
    SParsed parsed -> withTimer ("Resolving " ++ path) do
      updateFile path $ stage .~ SResolving
      (resolved, deps) <-
          flip runFileImport (LoadContext (dropFileName path) (Just path))
        $ resolveProgram builtinResolve parsed
      case resolved of
        Left es -> do
          updateFile path $ (stage .~ SUnresolved) . (dependencies .~ deps) . (errors . resolveErrors .~ es)
          runCallback path onResolved Nothing
          pure Nothing
        Right (ResolveResult resolved sig _) -> do
          runCallback path onResolved (Just (resolved, sig))
          updateFile path $ (stage .~ SResolved resolved sig) . (dependencies .~ deps)
          pure (Just sig)

    SUnresolved -> pure Nothing
    stage -> pure (Just (sig stage))

-- | Get or compute a file's type environment.
getTypeEnv :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => FilePath -> m (Maybe Env)
getTypeEnv path = do
  _ <- getSignature path

  file <- use (files . at path)
  case maybe SUnparsed (^.stage) file of
    SParsed{} -> pure Nothing
    SUnparsed{} -> pure Nothing
    SUnresolved{} -> pure Nothing
    SUntyped{} -> pure Nothing

    SResolving{} -> error "Impossible SResolving - should have been resolved."
    SResolved { _rBody = rBody, sig } -> do
      let ~(Just file') = file
      AllOf env <- foldMapM (fmap AllOf . getTypeEnv) (file' ^. dependencies)
      case env of
        -- One of our dependencies failed: Mark us as failed too and abort.
        Nothing -> updateFile path (stage .~ SUntyped sig) $> Nothing
        Just env -> withTimer ("Type checking " ++ path) do
          inferred <- inferProgram (builtinEnv <> env) =<< desugarProgram rBody
          let (res, es) = case inferred of
                That res -> (Just res, [])
                This es -> (Nothing, es)
                These es _ | any isError es -> (Nothing, es)
                These es res -> (Just res, es)

          case res of
            Nothing -> do
              updateFile path $ (stage .~ SUntyped sig) . (errors . typeErrors .~ es)
              runCallback path onTyped Nothing
              pure Nothing
            Just (tBody, modEnv) ->
              let env' = env
                    & (names %~ (<> (modEnv ^. names)))
                    . (types %~ (<> (modEnv ^. types)))
                    . (classDecs %~ (<> (modEnv ^. classDecs)))
                    . (modules %~ (<> (modEnv ^. modules))
                        . Map.insert (fileVar file') (modEnv ^. classes, modEnv ^. tySyms))
              in do updateFile path $ (stage .~ STyped tBody sig env') . (errors . typeErrors .~ es)
                    runCallback path onTyped (Just (tBody, env', builtinEnv <> env))
                    pure (Just env')

    stage -> pure (Just (env stage))

-- | Get or compute a file's "opened" type environment. Namely, the
-- environment as if it had been opened in the current scope.
getOpenedTypeEnv :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => FilePath -> m (Maybe Env)
getOpenedTypeEnv path = do
  env <- getTypeEnv path
  case env of
    Nothing -> pure Nothing
    Just env -> do
      ~(Just file) <- use (files . at path)
      let ~(Just (modImplicits, modTysym)) = env ^. modules . at (fileVar file)
      pure . Just $ env & (classes %~ (<>modImplicits)) . (tySyms %~ (<>modTysym))


-- | Determine whether a file can be successfully verified.
getVerified :: (MonadNamey m, MonadState Driver m, MonadIO m)
            => FilePath -> m Bool
getVerified path = do
  _ <- getTypeEnv path

  stg <- uses (files . at path) (maybe SUnparsed (^.stage))
  case stg of
    SParsed{} -> pure False
    SUnparsed{} -> pure False
    SUntyped{} -> pure False
    SUnresolved{} -> pure False

    SResolving{} -> error "Impossible SResolving - should have been resolved."
    SResolved{} -> error "Impossible SResolved - should have been typed."

    -- Already done
    SUnverified{} -> pure False
    SVerified{} -> pure True
    SEmitted{} -> pure True

    STyped prog sig env -> withTimer ("Verifying " ++ path) do
      v <- genName
      let (verified, errs) = verifyProg v env prog
      updateFile path $ (stage .~ if verified then SVerified prog sig env else SUnverified sig env)
                     . (errors %~ (<>errs))
      runCallback path onVerified (if verified then Just (prog, env) else Nothing)
      pure verified

getVerifiedAll :: (MonadNamey m, MonadState Driver m, MonadIO m)
               => FilePath -> m All
getVerifiedAll path = do
  here <- getVerified path

  deps <- uses (files . at path) (foldMap (^. dependencies))
  there <- foldMapM getVerifiedAll deps

  pure (All here <> there)

-- | Get or compute a file's lower state.
getLowerState :: (MonadNamey m, MonadState Driver m, MonadIO m)
              => FilePath -> m (Maybe LowerState)
getLowerState path = do
  ok <- getVerified path
  c <- use config

  if not ok then pure Nothing else do
    ~(Just file) <- use (files . at path)
    case file ^. stage of
      SEmitted _ _ _ lEnv -> pure (Just lEnv)
      SVerified prog sig env -> do
        AllOf lEnv <- foldMapM (fmap AllOf . getLowerState) (file ^. dependencies)
        case lEnv of
          Nothing -> pure Nothing
          _ | checkOnly c -> pure (Just mempty)
          Just lEnv -> withTimer ("Lowering for " ++ path) do
            (lEnv, l) <- runLowerWithEnv (defaultState <> lEnv) (lowerProgEnv prog)
            updateFile path $ stage .~ SEmitted l sig env lEnv
            uses config ((\f -> f path l) . onEmitted . callbacks) >>= liftIO
            pure (Just lEnv)
      _ -> error "Impossible: Should have been verified"

-- | Get or compute a file's core representation.
getLowered :: (MonadNamey m, MonadState Driver m, MonadIO m)
            => FilePath -> m (Maybe ([Stmt CoVar], LowerState))
getLowered path = do
  lEnv <- getLowerState path
  c <- use config
  case lEnv of
    Nothing -> pure Nothing
    _ | checkOnly c -> pure (Just (mempty, mempty))
    Just lEnv -> fmap ((,lEnv) . _cBody . (^.stage)) <$> use (files . at path)

-- | Get the errors for a specific file.
getErrors :: (MonadNamey m, MonadState Driver m)
          => FilePath -> m ErrorBundle
getErrors path = maybe mempty (^.errors) <$> use (files . at path)

-- | Get the errors for this file, and all dependencies.
getErrorsAll :: (MonadNamey m, MonadState Driver m)
          => FilePath -> m ErrorBundle
getErrorsAll = fmap fold . gatherDepsOf getErrors . Set.singleton

-- | Update an item within the state
updateFile :: MonadState Driver m
           => FilePath -> (LoadedFile -> LoadedFile) -> m ()
updateFile path f = files %= Map.update (Just . f) path

{- $importer

   LoadedFiles are originally located and inserted into the cache when
   running resolution. The process is relatively simple:

    - 'importFile': This attempts to load a file from the cache. If the
      file has not yet been loaded, then it will load it from disk
      instead.and pass it to the resolver. If the file is already being
      resolved when imported, then we determine that there is a cyclic
      import and error.

    - 'FileImport'/'runFileImport': Resolution is run within this
      monad. This is effectively 'importFile' specialised for a file - it
      supports relative paths, and tracks dependencies between modules.
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

instance (MonadNamey m, MonadState Driver m, MonadIO m) => MonadImport (FileImport m) where
  importModule loc relPath
    | not (T.pack "." `T.isPrefixOf` relPath)
    = FileIm \(LoadContext _ source) -> do
      libPath <- uses config libraryPath
      absPath <- liftIO $ findFile' (map (</> T.unpack relPath) libPath)
      case absPath of
        Nothing -> pure (NotFound, mempty)
        Just absPath -> (,Set.singleton absPath) <$> importFile source (Just loc) absPath

    | otherwise
    = FileIm \(LoadContext curDir source) -> do
      absPath <- liftIO $ canonicalizePath (curDir </> T.unpack relPath)
      (,Set.singleton absPath) <$> importFile source (Just loc) absPath

-- | Import a file from the current directory.
importFile :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => Maybe FilePath -> Maybe Span -> FilePath -> m ImportResult
importFile fromPath fromLoc path = withTimer ("Importing " ++ path) do
  file <- getFile path
  case file of
    Nothing -> pure NotFound
    Just file -> do
      case file ^. stage of
        SResolving -> do
          state <- get
          let ~(Just loc) = fromLoc
              fromFile = flip Map.lookup (state ^. files) =<< fromPath
          pure (ImportCycle ((fileSource file, loc) E.:| foldMap (`findCycle` state) fromFile))

        _ -> do
          updateFile path $ dependent %~ (<|> ((,) <$> fromPath <*> fromLoc))
          maybe Errored (Imported (fileVar file)) <$> getSignature path


-- | Find the first file which matches a list.
findFile' :: [FilePath] -> IO (Maybe FilePath)
findFile' [] = pure Nothing
findFile' (x:xs) = do
  path <- canonicalizePath x
  exists <- doesFileExist path
  if exists then pure (Just path) else findFile' xs

-- | Try to identify the cycle of files requiring each other.
findCycle :: LoadedFile -> Driver -> [(SourceName, Span)]
findCycle (LoadedFile { fileSource, _stage = SResolving, _dependent = Just (from, loc) }) st =
  (fileSource, loc) : findCycle (fromJust (Map.lookup from (st ^. files))) st
findCycle _ _ = []

newtype AllOf a = AllOf (Maybe a)

instance Semigroup a => Semigroup (AllOf a) where
  (AllOf (Just x)) <> (AllOf (Just y)) = AllOf (Just (x <> y))
  _ <> _ = AllOf Nothing

instance Monoid a => Monoid (AllOf a) where
  mempty = AllOf (Just mempty)

-- | Akin to 'sequenceA', this maps over a collection, accumulating the
-- results only if all functions return 'Just'.
foldMapM :: (Monad m, Foldable t, Monoid b)
         => (a -> m b) -> t a -> m b
foldMapM f = foldrM (\a b -> (<>b) <$> f a) mempty

isError :: Note a b => a -> Bool
isError x = diagnosticKind x == ErrorMessage

runCallback :: (MonadIO m, MonadState Driver m)
            => FilePath
            -> (DriverCallbacks -> FilePath -> a -> ErrorBundle -> IO ())
            -> a -> m ()
runCallback path fn x = do
  ~(Just file) <- use (files . at path)
  uses config ((\f -> f path x (file ^. errors)) . fn . callbacks) >>= liftIO
