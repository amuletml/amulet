-- | Utilities for working with Amulet files or paths
module Frontend.Files
  ( findFile'
  , foldMapM
  , buildLibraryPath
  ) where

import System.Environment
import System.Directory
import System.FilePath

import qualified Data.Text as T
import Data.List
import Data.Foldable

-- | Find the first file which matches a list.
findFile' :: [FilePath] -> IO (Either [FilePath] FilePath)
findFile' = go [] where
  go fs [] = pure . Left $ reverse fs
  go fs (x:xs) = do
    path <- canonicalizePath x
    exists <- doesFileExist path
    if exists then pure (Right path) else go (path:fs) xs

-- | Akin to 'sequenceA', this maps over a collection, accumulating the
-- results only if all functions return 'Just'.
foldMapM :: (Monad m, Foldable t, Monoid b) => (a -> m b) -> t a -> m b
foldMapM f = foldrM (\a b -> (<>b) <$> f a) mempty


-- | Get the defaultlibrary path.
--
-- This returns a path which will attempt to locate a file
-- from one of the "known directories" - $AMC_LIBRARY_PATH,
-- ../lib/ relative to the compiler's executable, or lib/
-- relative to the compiler's executable.
buildLibraryPath :: IO [FilePath]
buildLibraryPath = do
  execP <- getExecutablePath
  path <- foldMap splitColon <$> lookupEnv "AMC_LIBRARY_PATH"
  pure $ path ++
    [ takeDirectory execP </> "lib"
    , takeDirectory (takeDirectory execP) </> "lib"
    ]
  where
    splitColon = nub . map T.unpack . T.split (==':') . T.pack
