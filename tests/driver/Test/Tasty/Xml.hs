{-# LANGUAGE FlexibleContexts, OverloadedStrings, TupleSections, NamedFieldPuns #-}
module Test.Tasty.Xml (xmlReporter) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception

import qualified Data.IntMap as IntMap
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.Foldable
import Data.Monoid
import Data.Proxy

import Test.Tasty.Runners hiding (Ap(..))
import Test.Tasty.Providers
import Test.Tasty.Options

import qualified Text.XML.Light as Xml

import Type.Reflection

import System.Directory
import System.FilePath

newtype XmlFile = XmlFile (Maybe String)
  deriving (Show, Eq, Typeable)

instance IsOption XmlFile where
  optionName = "xml"
  optionHelp = "The JUnit XML file to write to."
  defaultValue = XmlFile Nothing
  parseValue = Just . XmlFile . Just

xmlReporter :: Ingredient
xmlReporter = TestReporter [Option (Proxy :: Proxy XmlFile)] run where
  run options tree =
    let (XmlFile file) = lookupOption options
    in runWith options tree <$> file

  runWith options tree outFile results = do
    results <- gatherSuites options tree results

    outFile <- canonicalizePath outFile
    createDirectoryIfMissing True (takeDirectory outFile)
    writeFile outFile . Xml.showTopElement . toXml $ results
    pure (const . pure $ True)

data TestCase = TestCase
  { testName      :: String
  , testClassName :: String
  , testResult    :: Result
  }

data TestSuite = TestSuite
  { suiteName     :: String
  , failures      :: Int
  , errors        :: Int
  , skipped       :: Int
  , time          :: Time
  , tests         :: [TestCase]
  }

gatherSuites :: OptionSet -> TestTree -> StatusMap -> IO [TestSuite]
gatherSuites options tree results
  = fmap (toList . makeGroup "Root")
  . flip evalStateT 0
  . flip runReaderT ""
  . getAp
  $ foldTestTree (trivialFold { foldSingle = test, foldGroup = group }) options tree
  where
    test :: (IsTest t, MonadIO m, MonadReader String m, MonadState Int m)
         => OptionSet -> TestName -> t -> Ap m (Seq TestCase, Seq TestSuite)
    test _ name _ = Ap $ do
      group <- ask
      ~(Just resVar) <- gets (`IntMap.lookup` results)
      result <- liftIO . atomically $ status retry pure =<< readTVar resVar
      modify (+1)

      pure ( pure (TestCase name (addGroup group name) result)
           , mempty )

    group :: MonadReader String m
          => OptionSet -> TestName -> Ap m (Seq TestCase, Seq TestSuite) -> Ap m (Seq TestCase, Seq TestSuite)
    group _ name children = Ap $ do
      group <- asks (`addGroup` name)
      (mempty,) . makeGroup group <$> local (const group) (getAp children)

    makeGroup :: String -> (Seq TestCase, Seq TestSuite) -> Seq TestSuite
    makeGroup _ (Seq.Empty, suites) = suites
    makeGroup name (children, suites) = suite Seq.<| suites where
      (Sum failures, Sum errors, Sum skipped, Sum time) = foldMap (getStats . testResult) children
      suite = TestSuite
        { suiteName = name
        , tests     = toList children
        , failures, errors, skipped, time
        }

    getStats (Result Success _ _ time _)                        = (0, 0, 0, Sum time)
    getStats (Result (Failure TestFailed) _ _ time _)           = (1, 0, 0, Sum time)
    getStats (Result (Failure TestThrewException{}) _ _ time _) = (0, 1, 0, Sum time)
    getStats (Result (Failure TestTimedOut{}) _ _ time _)       = (0, 1, 0, Sum time)
    getStats (Result (Failure TestDepFailed{}) _ _ time _)      = (0, 0, 1, Sum time)

    addGroup "" n = n
    addGroup g n = g ++ "." ++ n

    status :: a -> (Result -> a) -> Status -> a
    status x _ NotStarted = x
    status x _ Executing{} = x
    status _ f (Done x) = f x

toXml :: [TestSuite] -> Xml.Element
toXml = Xml.node (Xml.unqual "testsuites") . zipWith testSuite [(0 :: Int)..] where
  testSuite id suite = Xml.node (Xml.unqual "testsuite")
    ( [ Xml.Attr (Xml.unqual "package") (suiteName suite)
      , Xml.Attr (Xml.unqual "name") (suiteName suite)
      , Xml.Attr (Xml.unqual "id") (show id)
      , Xml.Attr (Xml.unqual "tests") (show (length (tests suite)))
      , Xml.Attr (Xml.unqual "failures") (show (failures suite))
      , Xml.Attr (Xml.unqual "errors") (show (errors suite))
      , Xml.Attr (Xml.unqual "skipped") (show (skipped suite))
      , Xml.Attr (Xml.unqual "time") (show (time suite))
      ]
    , map testCase (tests suite) )

  testCase tcase = Xml.node (Xml.unqual "testcase")
    ( [ Xml.Attr (Xml.unqual "name") (testName tcase)
      , Xml.Attr (Xml.unqual "classname") (testClassName tcase)
      , Xml.Attr (Xml.unqual "time") (show (resultTime (testResult tcase)))
      ], result (testResult tcase) )

  result (Result Success _ _ _ _) = []
  result (Result (Failure TestFailed) msg _ _ _) =
    [ Xml.node (Xml.unqual "failure") ([Xml.Attr (Xml.unqual "type") "Failure"], stripAnsi msg)]
  result (Result (Failure (TestThrewException (SomeException e))) msg _ _ _) =
    [ Xml.node (Xml.unqual "error")
      ( [ Xml.Attr (Xml.unqual "type") (show (typeOf e))
        , Xml.Attr (Xml.unqual "message") (displayException e)
        ], stripAnsi msg )]
  result (Result (Failure TestTimedOut{}) msg _ _ _) =
    [ Xml.node (Xml.unqual "error") ([Xml.Attr (Xml.unqual "type") "Timeout"], stripAnsi msg)]
  result (Result (Failure TestDepFailed{}) _ _ _ _) = [ Xml.node (Xml.unqual "skipped") () ]

  stripAnsi [] = []
  stripAnsi ('\x1b':xs) =
    case dropWhile (/='m') xs of
      ('m':xs) -> stripAnsi xs
      xs -> xs
  stripAnsi (x:xs) = x:stripAnsi xs
