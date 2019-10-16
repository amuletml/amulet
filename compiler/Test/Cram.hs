{-# LANGUAGE OverloadedStrings, TupleSections, LambdaCase #-}
{-| Support for Cram[1] tests within Tasty.

  Cram tests allow writing a series of commands, along with their
  expected outputs. They are effectively a golden test on an executable's
  output, but gathered into a single file.

  [1]: https://bitheap.org/cram/ -}
module Test.Cram (cram, cramDir) where

import Control.Monad

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Text.Lazy.IO as L
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Algorithm.Diff
import Data.Bifunctor
import Data.Proxy
import Data.Char

import Text.Regex.Base.RegexLike
import Text.Regex.PCRE.Text
import Text.Printf (printf)

import Test.Golden (Regenerate(..))
import Test.Tasty.Providers
import Test.Tasty.Options

import System.Process.Typed
import System.Directory
import System.FilePath

import Type.Reflection

newtype CramTest = CramTest FilePath
  deriving (Typeable)

data TestStep
  = Comment [T.Text]
  | Command T.Text [OutputLine]

-- | An expected output line, with text and expected pattern.
type OutputLine = (T.Text, OutputKind)

data OutputKind
  = Plain
  | Escaped T.Text
  | Regex Regex

-- | Escape any non-printable string
escape :: T.Text -> T.Text
escape = L.toStrict . B.toLazyText . T.foldr ((<>) . go) mempty where
  go c | isPrint c = B.singleton c
       | ord c <= 0xff = B.fromString (printf "\\x%02x" c)
       | otherwise = B.fromString (printf "\\u%08x" c)

-- | Determine if the expected output matches the actual one.
matches :: OutputLine -> T.Text -> Bool
matches (e, _) a | e == a = True
matches (_, Plain) _ = False
matches (_, Escaped e) a = e == escape a
matches (_, Regex e) a = matchTest e a

-- | Convert a string to an output line
toOutput :: T.Text -> OutputLine
toOutput t
  | T.all isPrint t = (t, Plain)
  | otherwise = (escape t <> "(esc)", Escaped (escape t))

-- | Parse the current test
parseTest :: L.Text -> Either String [TestStep]
parseTest = go . zip [1..] . L.lines where
  go :: [(Int, L.Text)] -> Either String [TestStep]
  go [] = pure mempty
  go ((n, l):ls)
    | L.isPrefixOf "  $ " l = do
      let (out, ls') = span (\(_, x) -> L.isPrefixOf "  " x && not (L.isPrefixOf "  $ " x)) ls
          out' = traverse (uncurry goOut . second (L.toStrict . L.drop 2)) out
      case out' of
        Left msg -> Left msg
        Right out' -> (Command (L.toStrict . L.drop 4 $ l) out':) <$> go ls'
    | L.isPrefixOf "  " l = Left ("Unexpected command output on line " ++ show n)
    | otherwise =
      let (comments, ls') = span (not . L.isPrefixOf "  " . snd) ls
      in (Comment (map (L.toStrict . snd) ((n, l):comments)):) <$> go ls'

  goOut :: Int -> T.Text -> Either String OutputLine
  goOut n l
    | T.isSuffixOf "(esc)" l = pure (l, Escaped (T.dropEnd 5 l))
    | T.isSuffixOf "(re)" l =
        case makeRegexOptsM compAnchored execAnchored (T.dropEnd 4 l) of
          Nothing -> Left ("Cannot compile regex on line " ++ show n)
          Just re -> pure (l, Regex re)
    | otherwise = pure (l, Plain)

formatTest :: [TestStep] -> L.Text
formatTest = B.toLazyText . go where
  go :: [TestStep] -> B.Builder
  go [] = mempty
  go (Comment t:ts) =
    foldr (\x b -> B.fromText x <> B.singleton '\n' <> b) (go ts) t
  go (Command c out:ts) =
    B.fromText "  $ " <> B.fromText c <> B.singleton '\n' <>
    foldr (\(x, _) b -> B.fromText "  " <> B.fromText x <> B.singleton '\n' <> b)
      (go ts) out

-- | Run tests until the first failure. Returns a possible diff in the
-- event of failures, and the "fixed" tests.
runTest :: [TestStep] -> IO (Maybe (T.Text, [PolyDiff OutputLine T.Text]), [TestStep])
runTest [] = pure (Nothing, [])
runTest (c@Comment{}:ts) = second (c:) <$> runTest ts
runTest (c@(Command command exp):ts) = do
  actual <- T.lines . L.toStrict . L.decodeUtf8 . snd <$> readProcessInterleaved (shell (T.unpack command))
  if check exp actual then second (c:) <$> runTest ts else
    let diff = getDiffBy matches exp actual in
    pure (Just (command, diff), Command command (ofDiff diff):ts)

  where
    check [] [] = True
    check (_:_) [] = False
    check [] (_:_) = False
    check (e:es) (a:as) = matches e a && check es as

    -- | Rebuild a list of output lines from a diff. The advantage here
    -- is that we will preserve regexes and escaped lines which match.
    ofDiff [] = []
    ofDiff (Both l _:d) = l : ofDiff d
    ofDiff (First _:d) = ofDiff d
    ofDiff (Second l:d) = toOutput l : ofDiff d

instance IsTest CramTest where
  run options (CramTest input) _ = do
    let Regenerate regenerate = lookupOption options
    test <- parseTest <$> L.readFile input
    case test of
      Left err -> pure $ testFailed err
      Right tests -> do
        (errs, tests') <- runTest tests
        case errs of
          Nothing -> pure $ testPassed ""
          Just (cmd, diff)-> do
            when regenerate (L.writeFile input (formatTest tests'))
            pure $ testFailed ("\27[0m $ " ++ T.unpack cmd ++ "\n" ++ T.unpack (formatDoc diff))

    where
      formatDoc :: [PolyDiff OutputLine T.Text] -> T.Text
      formatDoc [] = ""
      formatDoc (Both _ l:d)    =          " " <> l  <>       "\n" <> formatDoc d
      formatDoc (First (l,_):d) = "\27[1;31m-" <> l  <> "\27[0m\n" <> formatDoc d
      formatDoc (Second l:d)    = "\27[1;32m+" <> l' <> "\27[0m\n" <> formatDoc d
        where (l', _) = toOutput l

  testOptions = pure [ Option (Proxy :: Proxy Regenerate) ]

-- | Construct a test from a single @.t@ file.
cram :: FilePath -> TestTree
cram name = singleTest name (CramTest name)

-- | Construct multiple tests from a directory of @.t@ files.
cramDir :: FilePath -> IO [TestTree]
cramDir dir = map (cram . (dir</>)) . filter (isExtensionOf "t") <$> listDirectory dir
