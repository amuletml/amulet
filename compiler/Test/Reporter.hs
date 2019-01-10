{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Test.Reporter (boringReporter) where

import Control.Concurrent.STM
import Control.Monad.Reader
import Control.Monad.State

import qualified Data.IntMap as IntMap
import qualified Data.Text.IO as T
import Data.Sequence (Seq)
import Data.Foldable
import Data.Monoid
import Data.Proxy

import Test.Tasty.Ingredients.ConsoleReporter (UseColor(..))
import Test.Tasty.Ingredients
import Test.Tasty.Providers
import Test.Tasty.Runners hiding (Ap(..))
import Test.Tasty.Options
import Test.Options

import Text.Printf

import Text.Pretty.Ansi
import Text.Pretty hiding (displayDecorated, putDoc)

import Type.Reflection

import System.IO

data TestDisplay = Tests | Groups | None
  deriving (Show, Eq, Typeable)

instance IsOption TestDisplay where
  optionName = "display"
  optionHelp = "What information to output when running tests"
  defaultValue = None
  parseValue = parsePrefix
   [ ("tests", Tests)
   , ("groups", Groups)
   , ("none", None)
   ]

newtype Timing = Timing Bool
  deriving (Show, Eq, Typeable)

instance IsOption Timing where
  optionName = "timing"
  optionHelp = "Show times to run tests"
  defaultValue = Timing False
  parseValue = fmap Timing . safeReadBool
  optionCLParser = flagCLParser (Just 't') (Timing True)

boringReporter :: Ingredient
boringReporter
  = TestReporter
    [ Option (Proxy :: Proxy TestDisplay)
    , Option (Proxy :: Proxy Timing)
    , Option (Proxy :: Proxy UseColor)
    ] run where
  run options tree = Just (runReporter options tree)

runReporter :: OptionSet -> TestTree -> StatusMap -> IO (Time -> IO Bool)
runReporter options tree smap = do
  -- Print a dot for each of the tests
  hSetBuffering stdout NoBuffering
  results <- printProgress smap mempty
  putStrLn ""

  -- And print a summary of the results
  hSetBuffering stdout LineBuffering
  printResults results

  pure (const . pure $ foldr ((&&) . resultSuccessful) True results)

  where
    testDisplay = lookupOption options :: TestDisplay

    printProgress :: StatusMap -> IntMap.IntMap Result -> IO (IntMap.IntMap Result)
    printProgress tests results
      | null tests = pure results
      | otherwise = do
          -- Wait for at least one new result
          results' <- atomically $ do
            done <- IntMap.mapMaybe getResult <$> traverse readTVar tests
            if null done then retry
            else pure done
          traverse_ printProgressDot results'
          printProgress (tests `IntMap.difference` results') (results `IntMap.union` results')

    -- | Writes a progress dot to the terminal
    printProgressDot r = T.hPutStr stdout . displayDecorated . renderPretty 0.4 100 $ case resultOutcome r of
      Success -> annotate (DullColour Green) "•"
      Failure TestFailed -> annotate (DullColour Red) "◼"
      Failure _ -> annotate (DullColour Magenta) "✱"

    printResults :: IntMap.IntMap Result -> IO ()
    printResults results =
      let res = flip evalState 0
              . flip runReaderT 0
              . getAp
              $ foldTestTree (trivialFold { foldSingle = test, foldGroup = group })
                             options tree
      in putDoc (foldr (<##>) mempty (description res))

      where
        test :: (IsTest t, MonadState Int m)
             => OptionSet -> TestName -> t -> Ap m Results
        test _ test _ = Ap $ do
          ~(Just result) <- gets (`IntMap.lookup` results)
          modify (+1)
          pure Results { totalTests = 1
                       , successTests = if resultSuccessful result then 1 else 0
                       , time = resultTime result
                       , description = displayTest test result }

        group name (Ap r) = Ap $ do
          level <- ask
          r' <- local (+1) r
          pure (r' { description = displayGroup level name r' })

    displayTest :: TestName -> Result -> Seq (Doc AnsiStyle)
    displayTest name result
      -- If we're not displaying tests and we passed, then skip this
      | Tests /= testDisplay, Success <- resultOutcome result = mempty
      | otherwise = pure
        $ (case resultOutcome result of
            Success -> annotate (DullColour Green) "✓"
            Failure _ -> annotate (DullColour Red) "✘") <+> string name
       <> (case resultDescription result of
             "" -> mempty
             l -> nest 2 (line <> string l))

    displayGroup :: Int -> TestName -> Results -> Seq (Doc AnsiStyle)
    displayGroup level name (Results total success time desc)
      -- If we're displaying nothing, we've no children, and we're not
      -- the root then display nothing.
      | None <- testDisplay, null desc, level > 0 = mempty
      | otherwise = pure
        $ string name
       <> (if time <= 0.01 then mempty else
           annotate (DullColour Magenta) $ " took " <> string (printf "%.2fs" time))
       <> (if total == 0 then mempty else
           annotate (DullColour Cyan) $ " (" <> shown success <+> "out of" <+> shown total <+> "passed)")
       <> nest 2 (foldMap (line<>) desc)

data Results = Results
  { totalTests :: Int
  , successTests :: Int
  , time :: Time
  , description :: Seq (Doc AnsiStyle)
  }

instance Semigroup Results where
  (Results t s ti d) <> (Results t' s' ti' d') = Results (t + t') (s + s') (ti + ti') (d <> d')

instance Monoid Results where
  mempty = Results 0 0 0 mempty


-- | Extract the result from a status
getResult :: Status -> Maybe Result
getResult (Done r) = Just r
getResult _ = Nothing
