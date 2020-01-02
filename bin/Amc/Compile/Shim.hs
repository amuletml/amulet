{-# LANGUAGE TemplateHaskell #-}

module Amc.Compile.Shim (shim) where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

shim :: String
shim = $(do
  qAddDependentFile "bin/Amc/Compile/Shim.c"
  str <- qRunIO (readFile "bin/Amc/Compile/Shim.c")
  length str `seq` stringE str)

