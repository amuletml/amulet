{-# LANGUAGE OverloadedStrings #-}
module Backend.Lua.Syntax
  ( LuaStmt(..)
  , LuaVar(..)
  , LuaExpr(..)
  , keywords
  ) where

import Text.Pretty.Semantic

import qualified Data.Text.Lazy.Builder.Int as B
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as L
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char

-- | A Lua statement
data LuaStmt
  = LuaDo [LuaStmt]
  | LuaAssign [LuaVar] [LuaExpr]
  | LuaWhile LuaExpr [LuaStmt]
  | LuaRepeat [LuaStmt] LuaExpr
  | LuaIf LuaExpr [LuaStmt] [LuaStmt]
  | LuaFornum Text LuaExpr LuaExpr LuaExpr [LuaStmt]
  | LuaFor [Text] [LuaExpr] [LuaStmt]
  | LuaLocal [LuaVar] [LuaExpr]
  | LuaReturn LuaExpr
  | LuaIfElse [(LuaExpr, [LuaStmt])]
  | LuaBreak
  | LuaCallS LuaExpr [LuaExpr]
  | LuaBit Text
  deriving (Eq, Show, Ord)

-- | A variable which can be set on the left hand side of a binder
data LuaVar
  = LuaName Text
  | LuaIndex LuaExpr LuaExpr
  deriving (Eq, Show, Ord)

-- | A Lua expression
data LuaExpr
  = LuaCall LuaExpr [LuaExpr]
  | LuaNil | LuaTrue | LuaFalse | LuaDots
  | LuaRef LuaVar
  | LuaNumber Double
  | LuaInteger Int
  | LuaString Text
  | LuaFunction [LuaVar] [LuaStmt]
  | LuaTable [(LuaExpr, LuaExpr)]
  | LuaBinOp LuaExpr Text LuaExpr
  | LuaBitE Text
  deriving (Eq, Show, Ord)

body :: [LuaStmt] -> Doc
body = indent 2 . vsep . map pretty

instance Pretty LuaStmt where
  pretty (LuaDo xs) =
    vsep [ keyword "do"
         , body xs
         , keyword "end"
         ]
  pretty (LuaAssign ns xs) = hsep (punctuate comma (map pretty ns)) <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaWhile c t) =
    vsep [ keyword "while" <+> pretty c <+> keyword "do"
         , body t
         , keyword "end"
         ]
  pretty (LuaRepeat t c) =
    vsep [ keyword "repeat"
         , body t
         , keyword "until" <+> pretty c
         ]
  pretty (LuaIf c t []) =
    vsep [ keyword "if" <+> pretty c <+> keyword "then"
         , body t
         , keyword "end"
         ]
  pretty (LuaIf c t e) =
    vsep [ keyword "if" <+> pretty c <+> keyword "then"
         , body t
         , keyword "else"
         , body e
         , keyword "end"
         ]
  pretty (LuaIfElse ((c,t):bs)) =
    let pprintElse [] = keyword "end"
        pprintElse [(LuaTrue, b)] =
          vsep [ keyword "else"
               , body b
               , keyword "end"
               ]
        pprintElse ((c, b):xs) =
          vsep [ keyword "elseif" <+> pretty c <+> keyword "then"
               , body b
               ]
            <#> pprintElse xs
     in vsep [ keyword "if" <+> pretty c <+> keyword "then"
             , body t
             ]
        <#> pprintElse bs
  pretty (LuaIfElse []) = error "impossible"
  pretty (LuaFornum v s e i b) =
    vsep [ keyword "for" <+> text v <+> equals <+> keyword "do"
       <+> pretty s <+> comma <+> pretty e <+> comma <+> pretty i
         , body b
         , keyword "end"
         ]
  pretty (LuaFor vs es b) =
    vsep [ keyword "for" <+> hsep (punctuate comma (map text vs))
       <+> keyword "in" <+> hsep (punctuate comma (map pretty es))
       <+> keyword "do"
         , body b
         , keyword "end"
         ]
  pretty (LuaLocal [n] [LuaFunction a b]) =
    vsep [ keyword "local function" <+> pretty n <+> tupled (map pretty a)
         , body b
         , keyword "end"
         ]
  pretty (LuaLocal vs []) = keyword "local" <+> hsep (punctuate comma (map pretty vs))
  pretty (LuaLocal vs xs) = keyword "local" <+> hsep (punctuate comma (map pretty vs))
                        <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaBit x) = text x
  pretty LuaBreak = keyword "break"
  pretty (LuaReturn v) = keyword "return" <+> pretty v
  pretty (LuaCallS x@LuaFunction{} a) = parens (pretty x) <> tupled (map pretty a) <> semi
  pretty (LuaCallS x a) = pretty x <> tupled (map pretty a)

instance Pretty LuaVar where
  pretty (LuaName x) = text x
  pretty (LuaIndex e@(LuaRef _) (LuaString k))
    | validKey k = pretty e <> dot <> text k
  pretty (LuaIndex e (LuaString k))
    | validKey k = parens (pretty e) <> dot <> text k
  pretty (LuaIndex e k) = pretty e <> brackets (pretty k)

instance Pretty LuaExpr where
  pretty LuaTrue = sliteral (string "true")
  pretty LuaFalse = sliteral (string "false")
  pretty LuaDots = sliteral (string "...")
  pretty LuaNil = sliteral (string "nil")
  pretty (LuaString k) = sstring (dquotes (text (escapeStr k))) where
    escapeStr = L.toStrict . B.toLazyText . T.foldr (\x t -> escape x <> t) mempty
    escape '\n' = "\\n"
    escape '"' = "\\\""
    escape '\t' = "\\t"
    escape x | x < ' ' = "\\" <> B.decimal (ord x)
             | otherwise = B.singleton x
  pretty (LuaNumber d) = sliteral (pretty d)
  pretty (LuaInteger d) = sliteral (pretty d)
  pretty (LuaBinOp l o r) = pretty l <+> text o <+> pretty r
  pretty (LuaRef x) = pretty x
  pretty (LuaFunction a b) =
    vsep [ keyword "function" <+> tupled (map pretty a)
         , body b
         , keyword "end"
         ]
  pretty (LuaTable ps) = enclose (lbrace <> line) (line <> rbrace) . indent 2 . vsep . punctuate comma $
    map (\(k, v) -> key k <+> equals <+> pretty v) ps
    where key (LuaString k) | validKey k = text k
          key k = brackets (pretty k)
  pretty (LuaCall x@LuaFunction{} a) = parens (pretty x) <> tupled (map pretty a)
  pretty (LuaCall x a) = pretty x <> tupled (map pretty a)
  pretty (LuaBitE x) = text x

validKey :: Text -> Bool
validKey t = case T.uncons t of
               Nothing -> False
               Just (c, cs) -> start c && T.all rest cs && Set.notMember t keywords
  where
    start c = c == '_' || isAsciiUpper c || isAsciiLower c
    rest c = start c || isDigit c

-- | A set of all Lua keywords, which cannot be used as identifiers.
keywords :: Set.Set T.Text
keywords = Set.fromList [ "and", "break", "do", "else", "elseif", "end"
                        , "false", "for" , "function",  "if", "in", "local"
                        , "nil", "not", "or", "repeat", "return", "then"
                        , "true", "until", "while" ]
