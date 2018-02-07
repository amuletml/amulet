module Data.Position
  ( SourceName, Line, Column
  , SourcePos(..)
  ) where

type SourceName = String
type Column = Int
type Line = Int

data SourcePos = SourcePos { spFile :: SourceName
                           , spLine :: !Int
                           , spCol  :: !Int }
  deriving Show
