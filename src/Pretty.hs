{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
module Pretty
  ( module Text.PrettyPrint.Annotated.Leijen
  , Style
  , Pretty(..)
  , Doc
  , (<>), (<#>), (<##>)
  , putDoc, putDocWithoutColour, hPutDoc
  , render
  , text
  , skeyword, sliteral, sstring, stypeCon, stypeVar, stypeSkol, soperator

  , arrow, equals, colon, prod, pipe
  , keyword
  ) where


import qualified Text.PrettyPrint.Annotated.Leijen as P
import Text.PrettyPrint.Annotated.Leijen hiding (text, hPutDoc, putDoc, Doc, equals, colon, pipe, (<>), (<$>), (<$$>))
import Data.Semigroup

import System.IO (hPutStrLn, Handle, stdout)

import qualified Data.Text as T
import Data.Text (Text)

type Doc = P.Doc Style

instance Semigroup (P.Doc a) where
  (<>) = (P.<>)

instance Monoid (P.Doc a) where
  mempty = P.empty
  mappend = (P.<>)

class Pretty a where
  pretty :: a -> Doc

  prettyList :: [a] -> Doc
  prettyList = hsep . map pretty

data Style
  = Keyword
  | Literal
  | String

  | TypeCon
  | TypeVar
  | TypeSkol

  | Operator
  deriving (Eq, Show, Ord)

infixr 5 <#>,<##>

-- Alias various definitions so we're not masking existing definitions
(<#>), (<##>) :: P.Doc a -> P.Doc a -> P.Doc a
(<#>) = (P.<$>)
(<##>) = (P.<$$>)

text :: Text -> Doc
text = string . T.unpack

putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

putDocWithoutColour :: Doc -> IO ()
putDocWithoutColour = putStrLn . displayDecorated (flip const) . renderPretty 0.4 100

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h = hPutStrLn h . displayDecorated decorate . renderPretty 0.4 100 where

render :: Doc -> String
render = displayDecorated decorate . renderPretty 0.4 100

decorate :: Style -> String -> String
decorate Keyword s  = "\x1b[35m" ++ s ++ "\x1b[0m"
decorate Literal s  = "\x1b[1;33m" ++ s ++ "\x1b[0m"
decorate String s   = "\x1b[32m" ++ s ++ "\x1b[0m"
decorate TypeCon s  = "\x1b[34m" ++ s ++ "\x1b[0m"
decorate TypeVar s  = "\x1b[33m" ++ s ++ "\x1b[0m"
decorate TypeSkol s = "\x1b[35m" ++ s ++ "\x1b[0m"
decorate Operator s = "\x1b[35m" ++ s ++ "\x1b[0m"

instance Pretty Double where
  pretty = double

instance Pretty (P.Doc Style) where
  pretty = id

instance Pretty Char where
  pretty = char
  prettyList = string

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = prettyList

skeyword, sliteral, sstring, stypeCon, stypeVar, stypeSkol, soperator :: Doc -> Doc
skeyword = annotate Keyword . pretty
sliteral = annotate Literal . pretty
sstring = annotate String . pretty
stypeCon = annotate TypeCon . pretty
stypeVar = annotate TypeVar . pretty
stypeSkol = annotate TypeSkol . pretty
soperator = annotate Operator . pretty

pipe, arrow, equals, colon, prod :: Doc
arrow = soperator (string "->")
equals = soperator (char '=')
colon = soperator (char ':')
prod = soperator (char '*')
pipe = soperator (char '|')

keyword :: String -> Doc
keyword = skeyword . string
