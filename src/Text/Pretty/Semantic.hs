{-# LANGUAGE FlexibleInstances, GADTs #-}

{- | The semantic pretty printer is the primary method of displaying core
   compiler types, from "Syntax"'s expressions to "Core"'s terms and
   types.

   One should generate documents annotated with 'Style's, which can then
   be converted to ANSI or other formats.

   We also provide the 'Pretty' type class, which provides a more
   convenient way to display a value.
-}
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

  , arrow, equals, colon, prod, pipe, note
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

-- | A document annotated with the semantic 'Style'
type Doc = P.Doc Style

-- | A value which can be converted to a semantic document.
class Pretty a where
  -- | Convert 'a' into a document.
  pretty :: a -> Doc

  -- | Convert a list of 'a's into a document.
  prettyList :: [a] -> Doc
  prettyList = hsep . map pretty

-- | The base style for a semantic document. One should use the various
-- @s*@ methods to annotate a document instead.
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

-- | Render a document to stdout using ANSI escape sequences to colour
-- it.
putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

-- | Render a document to stdout without colouring it.
putDocWithoutColour :: Doc -> IO ()
putDocWithoutColour = T.putStrLn . P.display . renderAnsi

-- | Render a document to a given handle using ANSI escape sequences to
-- colour it.
hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h = T.hPutStrLn h . A.displayDecorated . renderAnsi

-- | Convert a document to a text value, using ANSI escape sequences to
-- colour it.
displayT :: Doc -> T.Text
displayT = A.displayDecorated . renderAnsi

-- | Convert a document to a string, using ANSI escape sequences to
-- colour it.
displayS :: Doc -> String
displayS = T.unpack . displayT

-- | An alternative version of 'displayT', which includes values
-- annotated with 'scomment'.
displayDetailed  :: Doc -> T.Text
displayDetailed = A.displayDecorated . renderAnsiDetailed

renderAnsi, renderAnsiDetailed :: Doc -> SimpleDoc AnsiStyle
renderAnsi = fmap toAnsi . uncommentDoc . renderPretty 0.4 100
renderAnsiDetailed = fmap toAnsi . renderPretty 0.4 100

-- | Remove all parts of the document annotated with 'scomment'.
uncommentDoc :: SimpleDoc Style -> SimpleDoc Style
uncommentDoc = filterSimpleDoc uncommentFilter

-- | A filter which removes any elements annotated with 'scomment'.
uncommentFilter :: Style -> Bool
uncommentFilter = (/=Comment)

-- | Convert a semantic style to an ANSI style.
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

-- | Construct a document with a specific annotation
skeyword, sliteral, sstring, scomment, stypeCon, stypeVar, stypeSkol, soperator :: Doc -> Doc
skeyword = annotate Keyword . pretty
sliteral = annotate Literal . pretty
sstring = annotate String . pretty
scomment = annotate Comment . pretty
stypeCon = annotate TypeCon . pretty
stypeVar = annotate TypeVar . pretty
stypeSkol = annotate TypeSkol . pretty
soperator = annotate Operator . pretty

-- | Various built-in operators
pipe, arrow, equals, colon, prod :: Doc
arrow = soperator (string "->")
equals = soperator (char '=')
colon = soperator (char ':')
prod = soperator (char '*')
pipe = soperator (char '|')

-- | Create a named keyword
keyword :: String -> Doc
keyword = skeyword . string

-- | Create a named type skolem
highlight :: String -> Doc
highlight = stypeSkol . string

-- | Pretty print a value wrapped in backticks, useful for displaying
-- literals or variable names.
verbatim :: Pretty a => a -> Doc
verbatim = enclose (char '`') (char '`') . pretty

-- | A prefix useful for denoting additional information in a message.
note :: P.Doc a
note = bullet (string "Note:")
