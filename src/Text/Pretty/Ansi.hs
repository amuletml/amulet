{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Text.Pretty.Ansi
  ( Colour(..)
  , AnsiStyle (..)
  , putDoc, putDocWithoutColour, hPutDoc, render
  , displayDecorated
  ) where

import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Maybe

import Text.Pretty hiding (putDoc, displayDecorated, hPutDoc)

import System.IO (Handle, stdout)

data Colour = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White
  deriving (Show, Eq, Ord, Enum)

data AnsiStyle
  = Unstyled
  | DullColour Colour
  | BrightColour Colour
  | Underlined
  | AnsiStyles [AnsiStyle]
  deriving (Show, Eq, Ord)

data TermState = TS { colour    :: Maybe Colour
                    , bold      :: Bool
                    , underline :: Bool
                    }
  deriving (Show, Eq)

putDoc :: Doc AnsiStyle -> IO ()
putDoc = hPutDoc stdout

putDocWithoutColour :: Doc AnsiStyle -> IO ()
putDocWithoutColour = T.putStrLn . display . renderPretty 0.4 100

hPutDoc :: Handle -> Doc AnsiStyle -> IO ()
hPutDoc h = T.hPutStrLn h . displayDecorated . renderPretty 0.4 100

render :: Doc AnsiStyle -> T.Text
render = displayDecorated . renderPretty 0.4 100

displayDecorated :: SimpleDoc AnsiStyle -> T.Text
displayDecorated = L.toStrict . B.toLazyText . go [TS Nothing False False] where
  genDelta :: TermState -> TermState -> [Int]
  genDelta f@(TS fc fb fu) t@(TS tc tb tu)
    -- Identical states require no difference
    | f == t = mempty
    -- If we went from bold to not bold, underlined to not underlined
    -- or from coloured to normal, then reset our state and continue.
    | (fb && not tb) ||
      (fu && not tu) ||
      (isJust fc && isNothing tc)
    = [0] ++ [ 4 | tu ] ++ [ 1 | tb ] ++
      maybe [] (pure . (+30) . fromEnum) tc
    -- Otherwise just emit the delta
    | otherwise
    = [ 4 | tu && not fu ] ++ [ 1 | tb && not fb ] ++
      [ 30 + fromEnum (fromJust (colour t)) | fc /= tc ]

  appAnn :: TermState -> AnsiStyle -> TermState
  appAnn s Unstyled         = s
  appAnn s Underlined       = s { underline = True }
  appAnn s (BrightColour c) = s { bold = True,  colour = Just c }
  appAnn s (DullColour c)   = s { bold = False, colour = Just c }
  appAnn s (AnsiStyles c)   = foldl appAnn s c

  toAnsi [] = mempty
  toAnsi xs = "\x1b[" <> go xs <> B.singleton 'm' where
    go [] = undefined
    go [x] = B.decimal x
    go (x:xs) = B.decimal x <> B.singleton ';' <> go xs

  go :: [TermState] -> SimpleDoc AnsiStyle -> B.Builder
  go [_]       SEmpty              = mempty
  go stk       (SChar c x)         = B.singleton c <> go stk x
  go stk       (SText _ str x)     = B.fromString str <> go stk x
  go stk       (SLine _ x@SLine{}) = B.singleton '\n' <> go stk x
  go stk       (SLine ind x)       = B.singleton '\n' <> indentation ind <> go stk x
  go stk@(p:_) (SAnnotStart ann x) = let n = appAnn p ann
                                     in toAnsi (genDelta p n) <> go (n:stk) x
  go (n:p:stk) (SAnnotStop x)      = toAnsi (genDelta n p) <> go (p:stk) x

  -- malformed documents
  go _ SAnnotStop{} = error "stack underflow"
  go _ SAnnotStart{} = error "stack underflow"
  go s SEmpty = error ("stack not consumed by rendering (" ++ show s ++ ")")

  indentation n | n <= 0 = mempty
                | otherwise = B.fromLazyText (L.replicate (fromIntegral n) (L.singleton ' '))
