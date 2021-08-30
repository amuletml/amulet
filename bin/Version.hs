{-# LANGUAGE TemplateHaskell, CPP #-}
module Version (amcVersion) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import System.Process
import System.Exit

gitRevision :: ExpQ
gitRevision = do
  addDependentFile ".git/logs/HEAD"
  [| $(stringE =<< runIO (init <$> version)) :: String  |]

  where
    version :: IO String
    version = do
      (exit, out, _) <- readProcessWithExitCode "git" ["rev-parse", "--short", "@"] ""
      pure $ case exit of
        ExitSuccess -> init out
        ExitFailure _ -> "??"


versionStr :: String
#if defined(VERSION_amuletml)
versionStr = VERSION_amuletml
#else
versionStr = "unknown"
#endif

amcVersion :: ExpQ
amcVersion = [| $(lift versionStr) ++ " (" ++ $gitRevision ++ ")" |]
