{-# LANGUAGE TemplateHaskell, CPP #-}
module Version (amcVersion) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import System.Process

gitRevision :: ExpQ
gitRevision = [| $(stringE =<< runIO (init <$> readProcess "git" ["rev-parse", "--short", "@"] "")) :: String  |]

versionStr :: String
versionStr = VERSION_amuletml

amcVersion :: ExpQ
amcVersion = [| $(lift versionStr) ++ " (" ++ $gitRevision ++ ")" |]
