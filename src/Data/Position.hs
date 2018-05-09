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
  deriving (Eq)

instance Show SourcePos where
  show (SourcePos f l c) = "[" ++ show (lim f f) ++ ":" ++ show l ++ ":" ++ show c ++ "]"
    where
      lim [] x = x
      lim ('/':xs) _ = lim xs xs
      lim (_:xs) x = lim xs x
