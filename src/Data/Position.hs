-- | Positions represent a single point or character within a file.
module Data.Position
  ( SourceName, Line, Column
  , SourcePos(..)
  ) where

import qualified Data.Text as T

-- | The type of file names
type SourceName = T.Text

-- | The type of column numbers
type Column = Int

-- | The type of line numbers
type Line = Int

-- | A point in the source code
data SourcePos = SourcePos { spFile :: SourceName -- ^ The file name of this position
                           , spLine :: !Int       -- ^ The line number (1-based)
                           , spCol  :: !Int       -- ^ The column number (1-based)
                           }
  deriving (Eq)

instance Show SourcePos where
  show (SourcePos f l c) = "[" ++ show (T.takeWhileEnd (/='/') f) ++ ":" ++ show l ++ ":" ++ show c ++ "]"
