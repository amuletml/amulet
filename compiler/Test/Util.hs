module Test.Util where

import Hedgehog
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Test.Tasty

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Algorithm.Diff
import Data.Maybe

import Control.Applicative
import Control.Exception

import System.Directory

hedgehog :: Group -> TestTree
hedgehog Group { groupName = n, groupProperties = ps }
  = testGroup (unGroupName n) (map (\(n, p) -> testProperty (unPropertyName n) p) ps)

golden :: FilePath -> String -> Assertion
golden expFile result = do
  expected <- (Just <$> readFile expFile) `catch` catchIO
  case expected of
    Nothing -> do
      writeFile expFile result
      assertFailure "File does not exist, generated"
    Just expected -> do
      let expected' = process expected
      let result' = process result
      if expected' /= result' then
        let contextDiff = getDiff expected' result'
        in assertFailure (formatDoc contextDiff)
      else pure ()

  where
    catchIO :: IOException -> IO (Maybe a)
    catchIO _ = pure Nothing

    process' [] = ([], [])
    process' ('\n':xs) = (process xs, [])
    process' (x:xs) = (x:) <$> process' xs
    process xs = let (ls, l) = process' xs in l:ls

    formatDoc :: [Diff String] -> String
    formatDoc [] = ""
    formatDoc (Both l _:d)  =           " " ++ l ++      "\n" ++ formatDoc d
    formatDoc (First l:d)   = "\27[1;31m-" ++ l ++ "\27[0m\n" ++ formatDoc d
    formatDoc (Second l:d)  = "\27[1;32m+" ++ l ++ "\27[0m\n" ++ formatDoc d

goldenFile :: (FilePath -> T.Text -> String) -> FilePath -> String -> String -> TestTree
goldenFile fn dir name out = testCase name $ do
  actual <- fn name <$> T.readFile (dir ++ name)
  golden (dir ++ out) actual

goldenDirOn :: (FilePath -> T.Text -> String) -> (String -> String) -> FilePath -> String -> IO [TestTree]
goldenDirOn fn out dir ext = mapMaybe (\x -> goldenFile fn dir x . out <$> spanTail ext x) <$> listDirectory dir where
  spanTail _ [] = Nothing
  spanTail s x@(y:ys) = if x == s then Just [] else (y:) <$> spanTail s ys

goldenDir :: (FilePath -> T.Text -> String) -> FilePath -> String -> IO [TestTree]
goldenDir = flip goldenDirOn (++".out")
