{-# LANGUAGE OverloadedStrings #-}
module Language.Scheme
  ( SchemeName(..)
  , Scheme(..)
  , SchemeStmt(..), leaf
  ) where


import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L

import Data.Sequence (Seq)
import qualified Data.Text as T
import Data.Char

import Text.Pretty.Semantic

-- | An identifier within Scheme.
newtype SchemeName = ScName T.Text deriving Show

-- | A scheme expression.
data Scheme
  = ScRef SchemeName
  | ScString T.Text
  | ScNumber Double
  | ScBoolean Bool
  | ScCall Scheme [Scheme]
  | ScLet [(SchemeName, [Scheme])] [Scheme]
  | ScIf Scheme Scheme Scheme
  | ScCond [(Scheme, [Scheme])]
  | ScLambda [SchemeName] [Scheme]
  deriving Show

instance Pretty SchemeName where
  pretty (ScName t) = text t

instance Pretty Scheme where
  pretty (ScRef x) = pretty x
  pretty (ScString x) = sstring . dquotes . text . escapeStr $ x where
    escapeStr = L.toStrict . B.toLazyText . T.foldr (\x t -> escape x <> t) mempty
    escape '\n' = "\\n"
    escape '"' = "\\\""
    escape '\t' = "\\t"
    escape '\\' = "\\\\"
    escape x | x < ' ' = "\\" <> B.decimal (ord x)
             | otherwise = B.singleton x
  pretty (ScNumber x) = sliteral (pretty x)
  pretty (ScBoolean x) = sliteral (if x then "#t" else "#f")

  pretty (ScCall f xs) = parens . hsep . map pretty $ (f:xs)
  pretty (ScLet binds rest) = parens . nest 2
    $ keyword "let"
    <+> ( parens . align . vsep . map (\(name, body) -> group . brackets . align $ pretty name <#> block body) $ binds )
    <#> block rest
  pretty (ScIf i t e) = parens (keyword "if" <+> pretty i <+> pretty t <+> pretty e)
  pretty (ScCond cases) = parens . nest 2 $
    keyword "cond"
    <#> vsep (map (\(test, body) -> group . brackets . align . vsep . map pretty $ (test:body)) cases)
  pretty (ScLambda args body) = parens . nest 2 $
    keyword "lambda" <+> (brackets . hsep . map pretty $ args)
    <#> block body


block :: [Scheme] -> Doc
block = vsep . map pretty

-- | A sequence of scheme expressions/terms. This allows building nested
-- terms (for instance, a let within a let) using semigroup style
-- constructs.
data SchemeStmt
  = Leaf (Seq Scheme)
  | Within (Seq Scheme -> Seq Scheme)
  | Empty

instance Semigroup SchemeStmt where
  Empty    <> x            = x
  x        <> Empty        = x
  (Leaf x) <> (Leaf y)     = Leaf (x <> y)
  (Within x) <> (Leaf y)   = Leaf (x y)
  (Leaf x) <> (Within y)   = Within (\r -> x <> y r)
  (Within x) <> (Within y) = Within (x . y)

instance Monoid SchemeStmt where
  mempty = Empty

leaf :: Scheme -> SchemeStmt
leaf = Leaf . pure
