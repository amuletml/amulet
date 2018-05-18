{-# OPTIONS_GHC -Wno-orphans #-}
module Text.Pretty
  ( module Text.PrettyPrint.Annotated.Leijen
  , (<>), (<#>), (<##>)
  , text, shown
  , bullet
  ) where


import qualified Text.PrettyPrint.Annotated.Leijen as P
import Text.PrettyPrint.Annotated.Leijen hiding (text, (<>), (<$>), (<$$>))

import qualified Data.Text as T
import Data.Text (Text)

instance Semigroup (Doc a) where
  (<>) = (P.<>)

instance Monoid (Doc a) where
  mempty = P.empty
  mappend = (P.<>)

infixr 5 <#>,<##>

-- Alias various definitions so we're not masking existing definitions
(<#>), (<##>) :: Doc a -> Doc a -> Doc a
(<#>) = (P.<$>)
(<##>) = (P.<$$>)

text :: Text -> Doc a
text = string . T.unpack

shown :: Show b => b -> Doc a
shown = string . show

bullet :: Doc a -> Doc a
bullet = (char '·'<+>)
