{-# LANGUAGE TemplateHaskell, CPP #-}
module Version (amcVersion) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import System.Process

gitRevision :: ExpQ
gitRevision = do
  addDependentFile ".git/logs/HEAD"
  [| $(stringE =<< runIO (init <$> readProcess "git" ["rev-parse", "--short", "@"] "")) :: String  |]

versionStr :: String
#if defined(VERSION_amuletml)
versionStr = VERSION_amuletml
#else
versionStr = "unknown"
#endif

amcVersion :: ExpQ
amcVersion = [| $(lift versionStr) ++ " (" ++ $gitRevision ++ ")" |]
