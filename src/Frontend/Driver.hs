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

  -- * REPL interaction
  --
  -- $repl
  , resolve, resolveWith
  , infer, inferWith
  , lower, lowerWith

  -- * Compilation
  --
  -- $compile
  , compile

  -- * Querying the driver
  --
  -- $query
  , getSignature, getTypeEnv
  , getVerified, getVerifiedAll
  , getLowerState, getLowered
  , loadPrelude
  ) where

import System.Environment
import System.Directory
import System.FilePath

import Control.Monad.State.Strict
import Control.Monad.Namey
import Control.Applicative
import Control.Exception
import Control.Lens hiding ((<.>))

import qualified Data.List.NonEmpty as E
import qualified Data.Map.Strict as Map
import qualified Data.Text.Lazy.IO as L
import qualified Data.Sequence as Seq
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Position
import Data.Foldable
import Data.Functor
import Data.Monoid
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
--
-- Note, while 'sig' represents the current module's signature, 'env' and
-- '_lowerState' are the /accumulated/ state of this module and all it
-- dependencies.
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
  deriving Show

data Module = Module
  { modLocation :: FilePath
  , modSource :: SourceName
  , modVar :: Name

  -- | Modules upon which this one depends.
  , _dependencies :: Set.Set FilePath
  -- | The first module upon which this one depends. Used for producing
  -- traces for cyclic dependencies.
  , _dependent :: Maybe (FilePath, Span)

  -- | The stage through the pipeline of this module.
  , _stage :: Stage

  , _errors :: ErrorBundle
  } deriving Show

data Driver = Driver
  { -- | All loaded modules
    _modules :: Map.Map FilePath Module
  -- | A mapping of pretty source paths, to cannonical file names.
  , _filePaths :: Map.Map SourceName FilePath
  } deriving Show

makeLenses ''Module
makeLenses ''Driver

-- | The default file driver.
emptyDriver :: Driver
emptyDriver = Driver mempty mempty

-- | Construct a file map, suitable for use with 'fileSpans' and other
-- "Text.Pretty.Note" functions.
fileMap :: MonadIO m => Driver -> m FileMap
fileMap driver
  = liftIO
  . traverse (\mod -> (modSource mod,) <$> T.readFile (modLocation mod))
  . Map.elems
  $ driver ^. modules

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
resolveWithDeps root parsed sig = do
  (resolved, deps) <- flip runFileImport (LoadContext root Nothing)
                    $ resolveProgram sig parsed
  (,deps) <$> case resolved of
    Left es -> pure (Nothing, mempty & (resolveErrors %~ (<>es)))
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
    Just r@(ResolveResult resolved _ _) -> do
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
  case res of
    Nothing -> (Nothing, ) . (errors<>) . fold <$> gatherDepsOf getErrors deps
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
      stg <- uses (modules . at path) (fmap (^.stage))
      case stg of
        Just (SEmitted _ _ _ lEnv) -> pure (Just ([], lEnv))
        _ -> getLowered path

{- $compile

   Various helper functions for compiling a whole bundle of files.
 -}

-- | Attempt to compile a single file. Returns the concatenated core of
-- all files.
compile :: (MonadNamey m, MonadIO m, MonadState Driver m)
        => FilePath -> m (Maybe [Stmt CoVar], ErrorBundle)
compile path = do
  l <- fmap (concat . fmap fst) . sequence <$> gatherDeps getLowered path
  errors <- fold <$> gatherDeps getErrors path
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
        deps <- uses (modules . at path) (foldMap (^.dependencies))
        (visited, seq) <- foldlM go (Set.insert path visited, seq) deps
        pure (visited, seq Seq.|> this)

-- | Walk over all nodes in dependency order. Effectively a preorder traversal.
gatherDeps :: (MonadNamey m, MonadState Driver m)
           => (FilePath -> m a)
           -> FilePath -> m (Seq.Seq a)
gatherDeps f = gatherDepsOf f . Set.singleton

verifyProg :: Name -> Env -> [Toplevel Typed] -> (Bool, ErrorBundle)
verifyProg v env inferred =
  let (ok, es) = runVerify env v (verifyProgram inferred)
  in (ok, mempty & verifyErrors .~ toList es)

{- $query

   We provide various methods for querying "the oracle". These take some
   /absolute/ file path, and perform whatever compilation steps required
   in order to produce the required output.

   All functions also return an 'ErrorBundle', holding any errors which
   occurred in the process of loading this module.
-}

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
getSignature :: (MonadNamey m, MonadState Driver m, MonadIO m)
             => FilePath -> m (Maybe Signature)
getSignature path = do
  mod <- use (modules . at path)
  mod <- case mod of
    Nothing -> do
      exists <- liftIO $ doesFileExist path
      if exists then Just <$> addModule path else pure Nothing
    Just mod -> pure (Just mod)

  case maybe SUnparsed (^. stage) mod of
    SUnparsed -> pure Nothing
    SParsed parsed -> do
      (resolved, deps) <-
          flip runFileImport (LoadContext (dropFileName path) (Just path))
        $ resolveProgram builtinResolve parsed
      case resolved of
        Left es -> do
          updateMod path $ (stage .~ SUnresolved) . (dependencies .~ deps) . (errors . resolveErrors .~ es)
          pure Nothing
        Right (ResolveResult resolved sig _) -> do
          updateMod path $ (stage .~ SResolved resolved sig) . (dependencies .~ deps)
          pure (Just sig)

    SUnresolved -> pure Nothing
    stage -> pure (Just (sig stage))

-- | Get or compute a module's type environment.
getTypeEnv :: (MonadNamey m, MonadState Driver m, MonadIO m)
           => FilePath -> m (Maybe Env)
getTypeEnv path = do
  _ <- getSignature path

  mod <- use (modules . at path)
  case maybe SUnparsed (^.stage) mod of
    SParsed{} -> pure Nothing
    SUnparsed{} -> pure Nothing
    SUnresolved{} -> pure Nothing
    SUntyped{} -> pure Nothing

    SResolved { _rBody = rBody, sig } -> do
      let ~(Just mod') = mod
      AllOf env <- foldMapM (fmap AllOf . getTypeEnv) (mod' ^. dependencies)
      case env of
        -- One of our dependencies failed: Mark us as failed too and abort.
        Nothing -> updateMod path (stage .~ SUntyped sig) $> Nothing
        Just env -> do
          inferred <- inferProgram (builtinEnv <> env) =<< desugarProgram rBody
          let (res, es) = case inferred of
                That res -> (Just res, [])
                This es -> (Nothing, es)
                These es _ | any isError es -> (Nothing, es)
                These es res -> (Just res, es)

          case res of
            Nothing -> do
              updateMod path $ (stage .~ SUntyped sig) . (errors . typeErrors .~ es)
              pure Nothing
            Just (tBody, modEnv) ->
              let env' = env
                    & (names %~ (<> (modEnv ^. names)))
                    . (types %~ (<> (modEnv ^. types)))
                    . (classDecs %~ (<> (modEnv ^. classDecs)))
                    . (classes %~ (<> (modEnv ^. classes)))
                    . (T.modules %~ (<> (modEnv ^. T.modules))
                        . Map.insert (modVar mod') (modEnv ^. classes, modEnv ^. tySyms))
              in do updateMod path $ (stage .~ STyped tBody sig env') . (errors . typeErrors .~ es)
                    pure (Just env')

    stage -> pure (Just (env stage))

-- | Determine whether a module can be successfully verified.
getVerified :: (MonadNamey m, MonadState Driver m, MonadIO m)
            => FilePath -> m Bool
getVerified path = do
  _ <- getTypeEnv path

  stg <- uses (modules . at path) (maybe SUnparsed (^.stage))
  case stg of
    SParsed{} -> pure False
    SUnparsed{} -> pure False
    SUntyped{} -> pure False
    SUnresolved{} -> pure False

    SResolved{} -> error "Impossible SResolved - should have been typed."

    -- Already done
    SUnverified{} -> pure False
    SVerified{} -> pure True
    SEmitted{} -> pure True

    STyped prog sig env -> do
      v <- genName
      let (verified, errs) = verifyProg v env prog
      updateMod path $ (stage .~ if verified then SVerified prog sig env else SUnverified sig env)
                     . (errors %~ (<>errs))
      pure verified

getVerifiedAll :: (MonadNamey m, MonadState Driver m, MonadIO m)
               => FilePath -> m All
getVerifiedAll path = do
  here <- getVerified path

  deps <- uses (modules . at path) (foldMap (^. dependencies))
  there <- foldMapM getVerifiedAll deps

  pure (All here <> there)

-- | Get or compute a module's lower state.
getLowerState :: (MonadNamey m, MonadState Driver m, MonadIO m)
              => FilePath -> m (Maybe LowerState)
getLowerState path = do
  ok <- getVerified path
  if not ok then pure Nothing else do
    ~(Just mod) <- use (modules . at path)
    case mod ^. stage of
      SEmitted _ _ _ lEnv -> pure (Just lEnv)
      SVerified prog sig env -> do
        AllOf lEnv <- foldMapM (fmap AllOf . getLowerState) (mod ^. dependencies)
        case lEnv of
          Nothing -> pure Nothing
          Just lEnv -> do
            (lEnv, l) <- runLowerWithEnv (defaultState <> lEnv) (lowerProgEnv prog)
            updateMod path $ stage .~ SEmitted l sig env lEnv
            pure (Just lEnv)
      _ -> error "Impossible: Should have been verified"

-- | Get or compute a module's core representation.
getLowered :: (MonadNamey m, MonadState Driver m, MonadIO m)
            => FilePath -> m (Maybe ([Stmt CoVar], LowerState))
getLowered path = do
  lEnv <- getLowerState path
  case lEnv of
    Nothing -> pure Nothing
    Just lEnv -> fmap ((,lEnv) . _cBody . (^.stage)) <$> use (modules . at path)

-- | Get the errors for a specific file.
getErrors :: (MonadNamey m, MonadState Driver m)
          => FilePath -> m ErrorBundle
getErrors path = maybe mempty (^.errors) <$> use (modules . at path)

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

instance (MonadNamey m, MonadState Driver m, MonadIO m) => MonadImport (FileImport m) where
  importModule loc relPath
    | not (T.pack "./" `T.isPrefixOf` relPath)
    = FileIm \(LoadContext _ source) -> do
      absPath <- findFile' (searchPath (T.unpack relPath))
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
importFile fromPath fromLoc path = do
  mod <- uses modules (Map.lookup path)
  case mod of
    Just mod -> do
      case mod ^. stage of
        SUnresolved -> pure Errored
        SUnparsed -> pure Errored
        SParsed _ -> do
          state <- get
          let ~(Just loc) = fromLoc
              fromMod = flip Map.lookup (state ^. modules) =<< fromPath
          pure (ImportCycle ((modSource mod, loc) E.:| foldMap (`findCycle` state) fromMod))

        stage -> pure (Imported (modVar mod) (sig stage))

    Nothing -> do
      exists <- liftIO $ doesFileExist path
      if not exists then pure NotFound else do
        mod <- addModule path
        updateMod path $ dependent .~ ((,) <$> fromPath <*> fromLoc)
        maybe Errored (Imported (modVar mod)) <$> getSignature path

-- | Find the first file which matches a list.
findFile' :: forall m. MonadIO m
          => [IO [FilePath]] -> m (Maybe FilePath)
findFile' = search where
  search :: [IO [FilePath]] -> m (Maybe FilePath)
  search [] = pure Nothing
  search (p:ps) = do
    paths <- liftIO p
    r <- searchMany paths
    case r of
      Just r -> pure (Just r)
      Nothing -> search ps

  searchMany (path:paths) = searchOne path (searchMany paths)
  searchMany [] = pure mempty

  searchOne path cont = do
    path <- liftIO $ canonicalizePath path
    exists <- liftIO $ doesFileExist path
    if exists then pure (Just path) else cont


-- | The default path to use with 'findFile''. This will attempt to locate
-- a file from one of the "known directories" - $AMC_LIBRARY_PATH, ../lib/
-- relative to the compiler's executable, or lib/ relative to the
-- compiler's executable.
searchPath :: FilePath -> [IO [FilePath]]
searchPath p =
  [ lookupEnv "AMC_LIBRARY_PATH" <&> fmap (\path -> map (</> p) (splitPath path))
                                 <&> msum
  , getExecutablePath <&> \execP -> [takeDirectory execP </> "lib" </> p]
  , getExecutablePath <&> \execP -> [takeDirectory (takeDirectory execP) </> "lib" </> p]
  ]
    where
      splitPath = Set.toList . Set.fromList . map T.unpack . T.split (==':') . T.pack

-- | Load the "prelude" module from a set of known locations:
-- * The AMC_PRELUDE environment variable (a file)
-- * $AMC_LIBRARY_PATH/prelude.ml
-- * ../lib/prelude.ml relative to the compiler's executable
-- * lib/prelude.ml relative to the compiler's executable
loadPrelude :: forall m.
               (MonadNamey m, MonadState Driver m, MonadIO m)
            => m (Signature, Env, [Stmt CoVar])
loadPrelude = load =<< findFile' ((toList <$> lookupEnv "AMC_PRELUDE") : searchPath "prelude.ml") where
  load Nothing = liftIO . throwIO . userError $ "Failed to locate Amulet prelude"
  load (Just p) = do
    r <- compile p
    case r of
      (Just stmts, _) -> do
        ~(Just sig) <- getSignature p
        ~(Just env) <- getTypeEnv p
        pure (sig, env, stmts)
      _ -> liftIO . throwIO . userError $ "Failed to load Amulet prelude from " ++ p

-- | Try to identify the cycle of modules requiring each other.
findCycle :: Module -> Driver -> [(FilePath, Span)]
findCycle (Module { modSource, _stage = SParsed _, _dependent = Just (from, loc) }) st =
  (modSource, loc) : findCycle (fromJust (Map.lookup from (st ^. modules))) st
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
