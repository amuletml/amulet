{-# LANGUAGE OverloadedStrings, FlexibleContexts, TupleSections #-}
module Test.Rerun (rerunning) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.Map.Strict as Map
import qualified Data.IntMap as IntMap
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Proxy

import System.IO.Error
import System.FilePath
import System.Directory

import Test.Tasty.Options
import Test.Tasty.Runners hiding (Ap(..))
import Test.Tasty.Providers

import Text.Read

import Type.Reflection

newtype RerunFile = RerunFile FilePath
  deriving (Show, Eq, Typeable)

instance IsOption RerunFile where
  optionName = "rerun-file"
  optionHelp = "The path to which the test runner's state should be saved"
  defaultValue = RerunFile "dist-newstyle/.rerun"
  parseValue = Just . RerunFile

data PreserveLog = PreserveLog | UpdateLog
  deriving (Show, Eq, Typeable)

instance IsOption PreserveLog where
  optionName = "rerun-preserve"
  optionHelp = "Do not update the rerun state file for this run"
  defaultValue = UpdateLog
  parseValue "preserve" = Just PreserveLog
  parseValue "update" = Just UpdateLog
  parseValue _ = Nothing
  optionCLParser = flagCLParser Nothing PreserveLog

data RerunFilter
  = RerunAll -- ^ Rerun all tests
  | RerunFailed -- ^ Only rerun tests which failed last time
  | RerunSame -- ^ Rerun whatever tests ran last time
  deriving (Show, Eq, Typeable)

instance IsOption RerunFilter where
  optionName = "rerun"
  optionHelp = "Rerun all tests which are new or failed in the last run."
  defaultValue = RerunAll
  parseValue "all" = Just RerunAll
  parseValue "failed" = Just RerunFailed
  parseValue "same" = Just RerunFailed
  parseValue _ = Nothing
  optionCLParser = flagCLParser (Just 'R') RerunFailed

data TestResult = ResultOk | ResultErr | ResultNotRun
  deriving (Read, Show)

type RerunState = Map.Map [TestName] TestResult

-- | Determine if the 'TestResult' matches the current 'RerunFilter'
resultMatches :: RerunFilter -> TestResult -> Bool
resultMatches RerunAll _ = True
resultMatches RerunFailed ResultErr = True
resultMatches RerunFailed _ = False
resultMatches RerunSame ResultNotRun = True
resultMatches RerunSame _ = False

-- | Allows rerunning the tests. One should wrap all other ingredients with this one.
rerunning :: [Ingredient] -> Ingredient
rerunning ingredients =
  TestManager
  ([ Option (Proxy :: Proxy RerunFile)
   , Option (Proxy :: Proxy PreserveLog)
   , Option (Proxy :: Proxy RerunFilter)
   ] ++ ingredientsOptions ingredients) (doRerun ingredients)

doRerun :: [Ingredient] -> OptionSet -> TestTree -> Maybe (IO Bool)
doRerun ingredients options testTree = Just $ do
  oldState <- loadState

  let
      filteredTestTree = maybe id (filterTestTree . filterMatches) oldState testTree

      tryIngredientMonitored :: Ingredient -> Maybe (IO Bool)
      tryIngredientMonitored (TestReporter _ f) = do
        runner <- f options filteredTestTree
        pure $ do
          (status, outcome) <- launchTestTree options filteredTestTree $ \sMap ->
              do f' <- runner sMap
                 pure (fmap (sMap,) . f')

          when (preserve == UpdateLog) $ do
            results <- getResults status
            let newState = extractState results filteredTestTree
            saveState newState

          pure outcome

      -- We can't actually monitor the tests in this case, so just run as normal.
      tryIngredientMonitored (TestManager _ f) = f options filteredTestTree

  -- If nobody ran the tests then ideally we'd return Nothing. However, we needed IO in
  -- order to read the previous state, so let's pretend we're all good.
  fromMaybe (pure True) (asum (map tryIngredientMonitored ingredients))

  where
    RerunFile stateFile = lookupOption options
    preserve = lookupOption options :: PreserveLog
    filter = lookupOption options :: RerunFilter

    -- | Determine if the provided test name matches the provided filter.
    filterMatches :: RerunState -> [TestName]  -> Bool
    filterMatches state = resultMatches filter . fromMaybe ResultNotRun . flip Map.lookup state

    -- | Load the state from a file
    loadState :: IO (Maybe RerunState)
    loadState =
      (readMaybe <$> readFile stateFile)
      `catchIOError` (\e -> if isDoesNotExistError e then pure Nothing else ioError e)

    -- | Save the state to a file
    saveState :: RerunState -> IO ()
    saveState results = do
      createDirectoryIfMissing True (takeDirectory stateFile)
      writeFile stateFile (show results)

    -- | Get all results for the given 'StatusMap'
    getResults :: StatusMap -> IO (IntMap.IntMap TestResult)
    getResults tests = atomically $ IntMap.map getResult <$> traverse readTVar tests

    getResult :: Status -> TestResult
    getResult (Done (Result Success _ _ _)) = ResultOk
    getResult (Done (Result Failure{} _ _ _)) = ResultErr
    getResult _ = ResultNotRun

    -- | Extract the 'RerunState' from the filtered test tree + extracted results.
    extractState :: IntMap.IntMap TestResult -> TestTree -> RerunState
    extractState results tree
      = flip evalState 0
      . flip runReaderT []
      . getAp
      $ foldTestTree (trivialFold { foldSingle = test, foldGroup = group }) options tree
      where
        test _ name _ = Ap $ do
          ~(Just result) <- gets (`IntMap.lookup` results)
          modify (+1)
          suf <- ask
          pure (Map.singleton (name:suf) result)

        group name (Ap r) = Ap $ local (name:) r


-- | Filters a test tree which matches a predicate.
filterTestTree :: ([TestName] -> Bool) -> TestTree -> TestTree
filterTestTree f = go' [] where
  go' x = fromMaybe (TestGroup "" []) . go x

  go :: [TestName] -> TestTree -> Maybe TestTree
  go suf t@(SingleTest name _) = if f (name:suf) then Just t else Nothing
  go suf (TestGroup name tests) =
    case mapMaybe (go (name:suf)) tests of
      [] -> Nothing
      tests' -> Just (TestGroup name tests')
  go suf (PlusTestOptions f t) = PlusTestOptions f <$> go suf t

  go suf (WithResource rSpec f) = Just (WithResource rSpec (go' suf <$> f))
  go suf (AskOptions f) = Just (AskOptions (go' suf <$> f))
