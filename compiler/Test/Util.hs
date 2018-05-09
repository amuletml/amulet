module Test.Util where

import Hedgehog
import Hedgehog.Internal.Property (unGroupName, unPropertyName)

import Test.Tasty.Hedgehog
import Test.Tasty.HUnit
import Test.Tasty

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Algorithm.Diff

import Control.Exception
import Data.List

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
    process xs = let (ls, l) = process' xs in dropWhileEnd null (l:ls)

    formatDoc :: [Diff String] -> String
    formatDoc [] = ""
    formatDoc (Both l _:d)  =           " " ++ l ++      "\n" ++ formatDoc d
    formatDoc (First l:d)   = "\27[1;31m-" ++ l ++ "\27[0m\n" ++ formatDoc d
    formatDoc (Second l:d)  = "\27[1;32m+" ++ l ++ "\27[0m\n" ++ formatDoc d

goldenFile :: (FilePath -> T.Text -> String) -> FilePath -> String -> TestTree
goldenFile fn dir name = testCase name $ do
  let file' = dir ++ name
  actual <- fn name <$> T.readFile file'
  golden (file' ++ ".out") actual
