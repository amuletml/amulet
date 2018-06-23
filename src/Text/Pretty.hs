{-# OPTIONS_GHC -Wno-orphans #-}

{- | A wrapper around "Text.PrettyPrint.Annotated.Leijen" which exposes an
   interface more in line with modern Haskell.

   This provides a way to build up documents using Wadler's combinators,
   which can then be rendered using 'renderPretty' and similar methods.
 -}
module Text.Pretty
  ( module Text.PrettyPrint.Annotated.Leijen
  , (<>), (<#>), (<##>)
  , text, shown
  , filterSimpleDoc
  , display
  , bullet
  ) where

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import qualified Text.PrettyPrint.Annotated.Leijen as P
import Text.PrettyPrint.Annotated.Leijen hiding (text, display, (<>), (<$>), (<$$>))

instance Semigroup (Doc a) where
  (<>) = (P.<>)

instance Monoid (Doc a) where
  mempty = P.empty
  mappend = (P.<>)

infixr 5 <#>,<##>

-- | Concatenates two documents with a 'line' between them.
(<#>) :: Doc a -> Doc a -> Doc a
(<#>) = (P.<$>)

-- | Concatenates two documents with a 'linebreak' between them.
(<##>) :: Doc a -> Doc a -> Doc a
(<##>) = (P.<$$>)

-- | Build a document from a text value.
text :: T.Text -> Doc a
text = string . T.unpack

-- | Build a document from some 'show'able value.
shown :: Show b => b -> Doc a
shown = string . show

-- | Prepend a bullet to a document.
bullet :: Doc a -> Doc a
bullet = (char '•'<+>)

-- | Filter a simple document, removing any regions whose annotation(s) do
-- not match a predicate.
--
-- This may be used to strip out additional metadata or comments.
filterSimpleDoc :: (a -> Bool) -> SimpleDoc a -> SimpleDoc a
filterSimpleDoc f = go where
  go SEmpty              = SEmpty
  go (SChar c x)         = SChar c (go x)
  go (SText l str x)     = SText l str (go x)
  go (SLine ind x)       = SLine ind (go x)
  go (SAnnotStart ann x) = if f ann then SAnnotStart ann (go x) else discard (0 :: Int) x
  go (SAnnotStop x)      = SAnnotStop (go x)

  discard _ SEmpty            = error "Unexpected empty"
  discard n (SChar _ x)       = discard n x
  discard n (SText _ _ x)     = discard n x
  discard n (SLine _ x)       = discard n x
  discard n (SAnnotStart _ x) = discard (n + 1) x
  discard n (SAnnotStop x)    | n == 0 = go x
                              | otherwise = discard (n - 1) x

-- | Render a 'SimpleDoc' as a text value
--
-- This is mostly the same as @annotated-wl-pprint@'s @display@, but it
-- avoids useless whitespace.
display :: SimpleDoc a -> T.Text
display = L.toStrict . B.toLazyText . go where
  go SEmpty              = mempty
  go (SChar c x)         = B.singleton c <> go x
  go (SText _ str x)     = B.fromString str <> go x
  go (SLine _ x@SLine{}) = B.singleton '\n' <> go x
  go (SLine ind x)       = B.singleton '\n' <> indentation ind <> go x
  go (SAnnotStart _ x)   = go x
  go (SAnnotStop x)      = go x

  indentation n | n <= 0 = mempty
                | otherwise = B.fromLazyText (L.replicate (fromIntegral n) (L.singleton ' '))
