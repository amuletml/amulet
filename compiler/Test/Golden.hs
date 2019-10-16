{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses, FlexibleContexts, NamedFieldPuns #-}
module Test.Golden
  ( Regenerate(..)
  , goldenFileM, goldenFile
  , goldenDirOnM, goldenDirOn
  , goldenDirM, goldenDir
  ) where

import Test.Tasty.Providers
import Test.Tasty.Options

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Algorithm.Diff
import Data.Proxy
import Data.Maybe
import Data.List

import Control.Exception
import Control.Monad

import System.Directory

import Type.Reflection

-- | Whether golden files should be regenerated if they don't match
newtype Regenerate = Regenerate Bool
  deriving (Show, Typeable)

instance IsOption Regenerate where
  defaultValue = Regenerate False
  parseValue = fmap Regenerate . safeReadBool
  optionName = "regenerate"
  optionHelp = "Regenerate golden files on mismatch"
  optionCLParser = flagCLParser (Just 'r') (Regenerate True)

data GoldenTest
  = GoldenTest
    { input  :: FilePath
    , output :: String
    , directory  :: String
    , generator  :: FilePath -> T.Text -> IO T.Text
    }
  deriving (Typeable)

instance IsTest GoldenTest where
  run options GoldenTest { input, output, directory, generator } _ = do
    let Regenerate regenerate = lookupOption options

    let fullOutput = directory ++ output

    actual <- generator input =<< T.readFile (directory ++ input)
    expected <- (Just <$> T.readFile fullOutput) `catch` catchIO

    case actual `seq` expected of
      Nothing -> do
        T.writeFile fullOutput actual
        pure (testFailed (unlines ("File does not exist, generated":lines (T.unpack actual))))
      Just expected -> do
        let expected' = process expected
        let result' = process actual
        if expected' /= result' then do
          when regenerate (T.writeFile fullOutput actual)

          let contextDiff = getDiff expected' result'
          pure (testFailed ("\27[0m" ++ T.unpack (formatDoc contextDiff)))
        else pure (testPassed "")

    where
      catchIO :: IOException -> IO (Maybe a)
      catchIO _ = pure Nothing

      process = T.split (=='\n') . T.dropAround (=='\n')

      formatDoc :: [Diff T.Text] -> T.Text
      formatDoc [] = ""
      formatDoc (Both l _:d)  =          " " <> l <>       "\n" <> formatDoc d
      formatDoc (First l:d)   = "\27[1;31m-" <> l <> "\27[0m\n" <> formatDoc d
      formatDoc (Second l:d)  = "\27[1;32m+" <> l <> "\27[0m\n" <> formatDoc d

  testOptions = pure [ Option (Proxy :: Proxy Regenerate) ]

goldenFileM :: (FilePath -> T.Text -> IO T.Text) -> FilePath -> String -> String -> TestTree
goldenFileM fn dir name out = singleTest name (GoldenTest name out dir fn)

goldenFile :: (FilePath -> T.Text -> T.Text) -> FilePath -> String -> String -> TestTree
goldenFile fn = goldenFileM (\f -> pure . fn f)

goldenDirOnM :: (FilePath -> T.Text -> IO T.Text) -> (String -> String) -> FilePath -> String -> IO [TestTree]
goldenDirOnM fn out dir ext = mapMaybe (\x -> goldenFileM fn dir x . out <$> spanTail ext x) . sort <$> listDirectory dir where
  spanTail _ [] = Nothing
  spanTail s x@(y:ys) = if x == s then Just [] else (y:) <$> spanTail s ys

goldenDirOn :: (FilePath -> T.Text -> T.Text) -> (String -> String) -> FilePath -> String -> IO [TestTree]
goldenDirOn fn = goldenDirOnM (\f -> pure . fn f)

goldenDirM :: (FilePath -> T.Text -> IO T.Text) -> FilePath -> String -> IO [TestTree]
goldenDirM = flip goldenDirOnM (++".out")

goldenDir :: (FilePath -> T.Text -> T.Text) -> FilePath -> String -> IO [TestTree]
goldenDir = flip goldenDirOn (++".out")
