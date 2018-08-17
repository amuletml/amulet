module Text.Pretty.Annotation where

import Text.Pretty.Semantic

class Annotation a where
  annotated :: a -> Doc -> Doc

instance Annotation () where
  annotated () = id
