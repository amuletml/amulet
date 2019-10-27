{-# LANGUAGE CPP, ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Types.Unify.Trace
  ( traceM
  , trace
  , traceShow
  , tracePrettyId
  , Component(..)
  ) where

#ifndef DISABLE_AMC_TRACE

{-# LANGUAGE Trustworthy #-}

import qualified System.IO.Unsafe as U
import System.Environment

import Data.Maybe
#endif

import Text.Pretty.Semantic

data Component = TopS | EquS | SubS | TcI | TcC | TcQL | TcB
  deriving (Eq, Read)

instance Show Component where
  show TopS = "⊤S"
  show EquS = "≡S"
  show SubS = "≤S"
  show TcI  = "⊢↑"
  show TcC  = "⊢↓"
  show TcB  = "Γ,"
  show TcQL = "QL"

traceM :: (Applicative m, Pretty a) => Component -> a -> m ()
trace :: Pretty x => Component -> x -> a -> a
traceShow :: Show a => Component -> a -> b -> b
tracePrettyId :: Pretty a => Component -> a -> a

-- This DEFINE only in production builds:
#ifdef DISABLE_AMC_TRACE

traceM _ _ = pure ()
trace _ _ x = x
traceShow _ _ x = x
tracePrettyId _ x = x

#else

{-# NOINLINE traceM #-}
{-# NOINLINE trace #-}
{-# NOINLINE traceShow #-}
{-# NOINLINE tracePrettyId #-}

traceM c s = U.unsafePerformIO (traceMaybe_s c (displayS (pretty s))) `seq` pure ()
trace c s x = U.unsafePerformIO (traceMaybe_s c (displayS (pretty s))) `seq` x 
traceShow c s x = U.unsafePerformIO (traceMaybe_s c (show s)) `seq` x
tracePrettyId c x = trace c (displayS (pretty x)) x


traceMaybe_s :: Component -> String -> IO ()
traceMaybe_s component string = do
  x <- fromMaybe "" <$> lookupEnv "AMC_TRACE"
  let cs = map read (words x)
      cs :: [Component]
  if component `elem` cs
     then putStrLn ("\x1b[41m" ++ show component ++ ":\x1b[0m " ++ string)
     else pure ()

#endif
