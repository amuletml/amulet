module Data.Position
  ( SourceName, Line, Column
  , SourcePos(..)
  ) where

-- | The type of file names
type SourceName = String

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
  show (SourcePos f l c) = "[" ++ show (lim f f) ++ ":" ++ show l ++ ":" ++ show c ++ "]"
    where
      lim [] x = x
      lim ('/':xs) _ = lim xs xs
      lim (_:xs) x = lim xs x
