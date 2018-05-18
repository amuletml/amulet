{-# LANGUAGE FlexibleInstances, GADTs #-}
module Text.Pretty.Semantic
  ( module Text.Pretty
  , Style
  , Pretty(..)
  , Doc

  , putDoc, putDocWithoutColour, hPutDoc
  , displayT, displayS, displayDetailed
  , renderAnsi, renderAnsiDetailed
  , uncommentDoc, uncommentFilter
  , toAnsi

  , skeyword, sliteral, sstring, scomment, stypeCon, stypeVar, stypeSkol, soperator

  , arrow, equals, colon, prod, pipe
  , keyword, highlight
  , verbatim
  ) where

import qualified Data.Text.IO as T
import qualified Data.Text as T

import Text.Pretty hiding (hPutDoc, putDoc, Doc, equals, colon, pipe, displayS)
import Text.Pretty.Ansi (Colour(..), AnsiStyle(..))
import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty as P

import System.IO (Handle, stdout)

type Doc = P.Doc Style

class Pretty a where
  pretty :: a -> Doc

  prettyList :: [a] -> Doc
  prettyList = hsep . map pretty

data Style
  = Keyword
  | Literal
  | String
  | Comment

  | TypeCon
  | TypeVar
  | TypeSkol

  | Operator
  deriving (Eq, Show, Ord)

putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

putDocWithoutColour :: Doc -> IO ()
putDocWithoutColour = T.putStrLn . P.display . renderAnsi

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h = T.hPutStrLn h . A.displayDecorated . renderAnsi

displayT :: Doc -> T.Text
displayT = A.displayDecorated . renderAnsi

displayS :: Doc -> String
displayS = T.unpack . displayT

displayDetailed  :: Doc -> T.Text
displayDetailed = A.displayDecorated . renderAnsiDetailed

renderAnsi, renderAnsiDetailed :: Doc -> SimpleDoc AnsiStyle
renderAnsi = fmap toAnsi . uncommentDoc . renderPretty 0.4 100
renderAnsiDetailed = fmap toAnsi . renderPretty 0.4 100

uncommentDoc :: SimpleDoc Style -> SimpleDoc Style
uncommentDoc = filterSimpleDoc uncommentFilter

uncommentFilter :: Style -> Bool
uncommentFilter = (/=Comment)

toAnsi :: Style -> AnsiStyle
toAnsi Keyword  = DullColour Magenta
toAnsi Literal  = BrightColour Yellow
toAnsi String   = DullColour Green
toAnsi Comment  = BrightColour Black
toAnsi TypeCon  = DullColour Blue
toAnsi TypeVar  = DullColour Yellow
toAnsi TypeSkol = DullColour Red
toAnsi Operator = DullColour Magenta

instance Pretty Double where
  pretty = double

instance a ~ Style => Pretty (P.Doc a) where
  pretty = id

instance Pretty Char where
  pretty = char
  prettyList = string

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty = prettyList

skeyword, sliteral, sstring, scomment, stypeCon, stypeVar, stypeSkol, soperator :: Doc -> Doc
skeyword = annotate Keyword . pretty
sliteral = annotate Literal . pretty
sstring = annotate String . pretty
scomment = annotate Comment . pretty
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

keyword, highlight :: String -> Doc
keyword = skeyword . string
highlight = stypeSkol . string

verbatim :: Pretty a => a -> Doc
verbatim = enclose (char '`') (char '`') . pretty
