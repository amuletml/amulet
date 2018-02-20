module Backend.Lua where

import Pretty

import qualified Data.Text as T
import Data.Text (Text)
import Data.Char

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

data LuaVar
  = LuaName Text
  | LuaIndex LuaExpr LuaExpr
  deriving (Eq, Show, Ord)

data LuaExpr
  = LuaCall LuaExpr [LuaExpr]
  | LuaNil | LuaTrue | LuaFalse | LuaDots
  | LuaRef LuaVar
  | LuaNumber Double
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
    vsep [ string "do"
         , body xs
         , string "end"
         ]
  pretty (LuaAssign ns xs) = hsep (punctuate comma (map pretty ns)) <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaWhile c t) =
    vsep [ string "while" <+> pretty c <+> string "do"
         , body t
         , string "end"
         ]
  pretty (LuaRepeat t c) =
    vsep [ string "repeat"
         , body t
         , string "until" <+> pretty c
         ]
  pretty (LuaIf c t []) =
    vsep [ string "if" <+> pretty c <+> string "then"
         , body t
         , string "end"
         ]
  pretty (LuaIf c t e) =
    vsep [ string "if" <+> pretty c <+> string "then"
         , body t
         , string "else"
         , body e
         , string "end"
         ]
  pretty (LuaIfElse ((c,t):bs)) =
    let pprintElse [] = string "end"
        pprintElse [(LuaTrue, b)] =
          vsep [ string "else"
               , body b
               , string "end"
               ]
        pprintElse ((c, b):xs) =
          vsep [ string "elseif" <+> pretty c <+> string "then"
               , body b
               ]
            <#> pprintElse xs
     in vsep [ string "if" <+> pretty c <+> string "then"
             , body t
             ]
        <#> pprintElse bs
  pretty (LuaIfElse []) = error "impossible"
  pretty (LuaFornum v s e i b) =
    vsep [ string "for" <+> text v <+> equals <+> string "do"
       <+> pretty s <+> comma <+> pretty e <+> comma <+> pretty i
         , body b
         , string "end"
         ]
  pretty (LuaFor vs es b) =
    vsep [ string "for" <+> hsep (punctuate comma (map text vs))
       <+> string "in" <+> hsep (punctuate comma (map pretty es))
       <+> string "do"
         , body b
         , string "end"
         ]
  pretty (LuaLocal [n] [LuaFunction a b]) =
    vsep [ string "local function" <+> pretty n <+> tupled (map pretty a)
         , body b
         , string "end"
         ]
  pretty (LuaLocal vs []) = string "local" <+> hsep (punctuate comma (map pretty vs))
  pretty (LuaLocal vs xs) = string "local" <+> hsep (punctuate comma (map pretty vs))
                        <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaBit x) = text x
  pretty LuaBreak = string "break"
  pretty (LuaReturn v) = string "return" <+> pretty v
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
  pretty LuaTrue = string "true"
  pretty LuaFalse = string "false"
  pretty LuaDots = string "..."
  pretty LuaNil = string "nil"
  pretty (LuaString k) = dquotes (text k)
  pretty (LuaNumber d) = pretty d
  pretty (LuaBinOp l o r) = pretty l <+> text o <+> pretty r
  pretty (LuaRef x) = pretty x
  pretty (LuaFunction a b) =
    vsep [ string "function" <+> tupled (map pretty a)
         , body b
         , string "end"
         ]
  pretty (LuaTable ps) = encloseSep lbrace rbrace comma $
    map (\(k, v) -> brackets (pretty k) <+> equals <+> pretty v) ps
  pretty (LuaCall x@LuaFunction{} a) = parens (pretty x) <> tupled (map pretty a)
  pretty (LuaCall x a) = pretty x <> tupled (map pretty a)
  pretty (LuaBitE x) = text x

validKey :: Text -> Bool
validKey t = case T.uncons t of
               Nothing -> False
               Just (c, cs) -> start c && T.all rest cs
  where
    start c = c == '_' || isAsciiUpper c || isAsciiLower c
    rest c = start c || isDigit c
