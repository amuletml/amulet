{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, PatternSynonyms #-}
module Language.Lua.Syntax
  ( LuaStmt(..), pattern LuaIf
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
import Data.Generics hiding (empty)
import Data.Char

-- | A Lua statement
data LuaStmt
  = LuaDo [LuaStmt]
  | LuaAssign [LuaVar] [LuaExpr]
  | LuaWhile LuaExpr [LuaStmt]
  | LuaRepeat [LuaStmt] LuaExpr
  | LuaFornum LuaVar LuaExpr LuaExpr LuaExpr [LuaStmt]
  | LuaFor [LuaVar] [LuaExpr] [LuaStmt]
  | LuaLocal [LuaVar] [LuaExpr]
  | LuaLocalFun LuaVar [LuaVar] [LuaStmt]
  | LuaReturn [LuaExpr]
  | LuaIfElse [(LuaExpr, [LuaStmt])]
  | LuaBreak
  | LuaCallS LuaExpr [LuaExpr]
  | LuaQuoteS Text
  deriving (Eq, Show, Ord, Typeable, Data)

-- | A shorthand for a basic @if@/@else@ constructor
pattern LuaIf :: LuaExpr -> [LuaStmt] -> [LuaStmt] -> LuaStmt
pattern LuaIf c t f = LuaIfElse [(c, t), (LuaTrue, f)]

-- | A variable which can be set on the left hand side of a binder
data LuaVar
  = LuaName Text
  | LuaIndex LuaExpr LuaExpr
  | LuaQuoteV Text
  deriving (Eq, Show, Ord, Typeable, Data)

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
  | LuaQuoteE Text
  deriving (Eq, Show, Ord, Typeable, Data)

-- | Emit an indented block of objects, with a header and footer
--
-- We have this weird asymmetry of 'line' as we need to indent the first line
-- line of the body but don't want to indent the footer.
block :: Doc -> [Doc] -> Doc -> Doc
block header body footer = header <> nest 2 (line <> vsep body) <> line <> footer

-- | A variant of 'block' but with an empty footer
headedBlock :: Doc -> [Doc] -> Doc
headedBlock header body = block header body empty

-- | Build a series of function arguments
args :: [Doc] -> Doc
args = parens . hsep . punctuate comma

instance Pretty LuaStmt where
  pretty (LuaDo xs) =
    block (keyword "do")
          (map pretty xs)
          (keyword "end")
  pretty (LuaAssign ns xs) = hsep (punctuate comma (map pretty ns)) <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaWhile c t) =
    block (keyword "while" <+> pretty c <+> keyword "do")
          (map pretty t)
          (keyword "end")
  pretty (LuaRepeat t c) =
    block (keyword "repeat")
          (map pretty t)
          (keyword "until" <+> pretty c)
  pretty (LuaIfElse ((c,t):bs)) =
    let pprintElse [] = keyword "end"
        pprintElse [(LuaTrue, b)] =
             headedBlock (keyword "else") (map pretty b)
          <> keyword "end"
        pprintElse ((c, b):xs) =
             headedBlock (keyword "elseif" <+> pretty c <+> keyword "then")
                         (map pretty b)
          <> pprintElse xs
     in headedBlock (keyword "if" <+> pretty c <+> keyword "then")
                    (map pretty t)
     <> pprintElse bs
  pretty (LuaIfElse []) = error "impossible"
  pretty (LuaFornum v s e i b) =
    block ( keyword "for" <+> pretty v <+> equals <+> keyword "do"
        <+> pretty s <+> comma <+> pretty e <+> comma <+> pretty i )
          (map pretty b)
          (keyword "end")
  pretty (LuaFor vs es b) =
    block ( keyword "for" <+> hsep (punctuate comma (map pretty vs))
        <+> keyword "in" <+> hsep (punctuate comma (map pretty es))
        <+> keyword "do" )
         (map pretty b)
         (keyword "end")
  pretty (LuaLocalFun n a b) =
    funcBlock (keyword "local function" <+> pretty n <> args (map pretty a))
              b
              (keyword "end")
  pretty (LuaLocal vs []) = keyword "local" <+> hsep (punctuate comma (map pretty vs))
  pretty (LuaLocal vs xs) = keyword "local" <+> hsep (punctuate comma (map pretty vs))
                        <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaQuoteS x) = "@" <> text x
  pretty LuaBreak = keyword "break"
  pretty (LuaReturn v) = keyword "return" <+> pretty v
  pretty (LuaCallS x@LuaFunction{} a) = parens (pretty x) <> args (map pretty a) <> semi
  pretty (LuaCallS x a) = pretty x <> args (map pretty a)

instance Pretty LuaVar where
  pretty (LuaName x) = text x
  pretty (LuaIndex e@(LuaRef _) (LuaString k))
    | validKey k = pretty e <> dot <> text k
  pretty (LuaIndex e (LuaString k))
    | validKey k = parens (pretty e) <> dot <> text k
  pretty (LuaIndex e k) = pretty e <> brackets (pretty k)
  pretty (LuaQuoteV x) = "$" <> text x

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
    funcBlock (keyword "function" <> args (map pretty a))
              b
              (keyword "end")
  pretty (LuaTable []) = lbrace <> rbrace
  pretty (LuaTable ps) = group (block lbrace (punctuate comma . entries 1 $ ps) rbrace) where
    entries _ [] = []
    entries n ((LuaString k, v):es) | validKey k = text k <+> value v : entries n es
    entries n ((LuaInteger k, v):es) | k == n = pretty v : entries (n + 1) es
    entries n ((k,v):es) = brackets (pretty k) <+> value v : entries n es

    value v = equals <+> pretty v
  pretty (LuaCall x@LuaFunction{} a) = parens (pretty x) <> args (map pretty a)
  pretty (LuaCall x a) = pretty x <> args (map pretty a)
  pretty (LuaQuoteE x) = "%" <> text x

-- | An alternative to 'block' which may group simple functions onto one line
funcBlock :: Doc -> [LuaStmt] -> Doc -> Doc
funcBlock header [] = group . block header []
funcBlock header [r@LuaReturn{}] = group . block header [pretty r]
funcBlock header body = block header (map pretty body)

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
