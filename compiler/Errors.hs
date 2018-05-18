{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-import-lists #-}
module Errors where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position

import "amuletml" Text.Pretty.Semantic
import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty.Note as N

reportS :: N.Note a Style => a -> [(SourceName, T.Text)] -> IO ()
reportS err fs = T.putStrLn
                 . A.displayDecorated
                 . fmap (either N.toAnsi toAnsi)
                 . filterSimpleDoc (either (const True) uncommentFilter)
                 . renderPretty 0.4 100
                 . N.format (N.fileSpans fs) $ err
