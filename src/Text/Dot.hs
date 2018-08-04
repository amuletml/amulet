{-# LANGUAGE OverloadedStrings #-}
module Text.Dot
  ( Graph(..)
  , GraphKind(..)
  , GraphElement(..)
  , ElemStyle(..)
  , ElemInfo(..)
  , defaultInfo
  , drawGraph
  , displayGraph
  ) where

import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.Char

import Text.Pretty

data Graph a = Graph GraphKind [GraphElement a]

data GraphKind = DirectedGraph | UndirectedGraph

data ElemStyle = Solid | Dashed | Dotted
  deriving (Show, Eq, Ord)

data ElemInfo =
  ElemInfo
  { label :: Maybe T.Text
  , style :: Maybe ElemStyle
  }
  deriving (Show)

defaultInfo :: ElemInfo
defaultInfo = ElemInfo Nothing Nothing

data GraphElement a
  = Node ElemInfo a
  | Edge ElemInfo a a

  | Subgraph [GraphElement a]

drawGraph :: (a -> Doc b) -> Graph a -> Doc b
drawGraph disp (Graph kind nodes) = name <+> "{" <#> vsep (map (indent 2 . elem) nodes) <#> "}"
  where
    name, sep :: Doc b
    (name, sep) = case kind of
             DirectedGraph -> ("digraph", "->")
             UndirectedGraph -> ("graph", "--")

    elem (Node info a) = disp a <> elemInfo info <> ";"
    elem (Edge info a b) = disp a <+> sep <+> disp b <> elemInfo info <> ";"
    elem (Subgraph nodes) = "subgraph {" <#> vsep (map (indent 2 . elem) nodes) <#> "}"

    elemInfo info = case elemAttributes info of
                      [] -> empty
                      atr -> empty <+> "[" <+> hsep (punctuate comma atr) <+> "]"

    elemAttributes (ElemInfo label style)
      = maybe id ((:) . (\label -> "label=\"" <> text (escapeStr label) <> "\"")) label
      . maybe id ((:) . (\style -> "style=" <> text (showStyle style))) style
      $ []

    escapeStr = L.toStrict . B.toLazyText . T.foldr (\x t -> escape x <> t) mempty
    escape '\n' = "\\n"
    escape '"' = "\\\""
    escape '\t' = "\\t"
    escape x | x < ' ' || x > '~' = "\\" <> B.decimal (ord x)
             | otherwise = B.singleton x

    showStyle Solid = "solid"
    showStyle Dashed = "dashed"
    showStyle Dotted = "dotted"

displayGraph :: (a -> Doc b) -> Graph a -> T.Text
displayGraph disp = display . renderPretty 0.4 100 . drawGraph disp
