{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, PatternSynonyms #-}
module Language.Lua.Syntax
  ( LuaStmt(..), pattern LuaIf
  , LuaVar(..)
  , LuaExpr(..)
  , LuaCall(..)
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
  | LuaCallS LuaCall
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
  = LuaCallE LuaCall
  | LuaNil | LuaTrue | LuaFalse | LuaDots
  | LuaRef LuaVar
  | LuaNumber Double
  | LuaInteger Int
  | LuaString Text
  | LuaFunction [LuaVar] [LuaStmt]
  | LuaTable [(LuaExpr, LuaExpr)]
  | LuaBinOp LuaExpr Text LuaExpr
  | LuaUnOp Text LuaExpr
  | LuaQuoteE Text
  | LuaBitE Text
  deriving (Eq, Show, Ord, Typeable, Data)

data LuaCall
  = LuaCall LuaExpr [LuaExpr]
  | LuaInvoke LuaExpr Text [LuaExpr]
  deriving (Eq, Show, Ord, Typeable, Data)

data Precedence
  = PreRaw    -- ^ Raw expressions which never need to be wrapped variables, calls, etc...
  | PreLit    -- ^ Literals, which should be wrapped when called
  | PreOp Int -- ^ Precedence for operators
  | PreWrap   -- ^ Expressions which always should be wrapped
  deriving (Eq, Ord, Show)

data Associativity = ALeft | ARight
  deriving (Eq, Show)

precedenceOf :: LuaExpr -> Precedence
precedenceOf LuaCallE{} = PreRaw
precedenceOf LuaRef{} = PreRaw
precedenceOf (LuaBinOp _ op _) = PreOp $ case op of
  -- https://www.lua.org/manual/5.3/manual.html#3.4.8
  "^" -> 0
  "*" -> 2; "/" -> 2; "%" -> 2
  "+" -> 3; "-" -> 3
  ".." -> 4
  "<<" -> 5; ">>" -> 5
  "&" -> 6
  "~" -> 7
  "|" -> 8
  "==" -> 9; "~=" -> 9; "<" -> 9; ">" -> 9; ">=" -> 9; "<=" -> 9
  "and" -> 10
  "or" -> 11
  _ -> 12
precedenceOf LuaUnOp{} = PreOp 1
precedenceOf _ = PreLit

decrPrec :: Precedence -> Precedence
decrPrec PreRaw    = PreRaw
decrPrec PreLit    = PreLit
decrPrec (PreOp x) = PreOp (x - 1)
decrPrec PreWrap   = PreWrap

assocOf :: T.Text -> Associativity
assocOf ".." = ARight
assocOf "^"  = ARight
assocOf _    = ALeft

contained :: Doc -> [Doc] -> Doc -> Doc
contained header body footer = header <> nest 2 (line <> vsep body) <> line <> footer

-- | Emit an indented block of statements, with a header and footer
--
-- We have this weird asymmetry of 'line' as we need to indent the first line
-- line of the body but don't want to indent the footer.
block :: Doc -> [LuaStmt] -> Doc -> Doc
block header body footer = header <> nest 2 (line <> stmts body) <> line <> footer

stmts :: [LuaStmt] -> Doc
stmts [] = mempty
stmts [x] = pretty x
stmts (a:b:cs)
  | trailingExpr a && leadingS b = pretty a <> ";" <#> stmts (b:cs)
  | otherwise = pretty a <#> stmts (b:cs) where
  trailingExpr LuaAssign{} = True
  trailingExpr LuaRepeat{} = True
  trailingExpr LuaReturn{} = True
  trailingExpr LuaLocal{} = True
  trailingExpr LuaCallS{} = True
  trailingExpr _ = False

  leadingS (LuaCallS c) = leadingC c
  leadingS (LuaAssign (v:_) _) = leadingVar v
  leadingS _ = False

  leadingC (LuaCall e _) = leadingFn e
  leadingC (LuaInvoke e _ _) = leadingFn e

  leadingFn (LuaCallE c) = leadingC c
  leadingFn (LuaRef LuaName{}) = False
  leadingFn _ = True

  leadingVar LuaName{} = False
  leadingVar (LuaIndex (LuaRef v) _) = leadingVar v
  leadingVar (LuaIndex _ _) = True
  leadingVar LuaQuoteV{} = False

-- | A variant of 'block' but with an empty footer
headedBlock :: Doc -> [LuaStmt] -> Doc
headedBlock header body = block header body empty

-- | Build a series of function arguments
args :: [Doc] -> Doc
args = parens . hsep . punctuate comma

instance Pretty LuaStmt where
  pretty (LuaDo xs) =
    block (keyword "do")
          xs
          (keyword "end")
  pretty (LuaAssign ns xs) = hsep (punctuate comma (map pretty ns)) <+> equals <+> hsep (punctuate comma (map pretty xs))
  pretty (LuaWhile c t) =
    block (keyword "while" <+> pretty c <+> keyword "do")
          t
          (keyword "end")
  pretty (LuaRepeat t c) =
    block (keyword "repeat")
          t
          (keyword "until" <+> pretty c)
  pretty (LuaIfElse [(c,[t])]) =
    group $ block (keyword "if" <+> pretty c <+> keyword "then") [t] (keyword "end")
  pretty (LuaIfElse ((c,t):bs)) =
    let pprintElse [] = keyword "end"
        pprintElse [(LuaTrue, b)] =
             headedBlock (keyword "else") b
          <> keyword "end"
        pprintElse ((c, b):xs) =
             headedBlock (keyword "elseif" <+> pretty c <+> keyword "then")
                         b
          <> pprintElse xs
     in headedBlock (keyword "if" <+> pretty c <+> keyword "then")
                    t
     <> pprintElse bs
  pretty (LuaIfElse []) = error "impossible"
  pretty (LuaFornum v s e i b) =
    block ( keyword "for" <+> pretty v <+> equals
        <+> pretty s <+> comma <+> pretty e <+> comma <+> pretty i <+> keyword "do" )
          b
          (keyword "end")
  pretty (LuaFor vs es b) =
    block ( keyword "for" <+> hsep (punctuate comma (map pretty vs))
        <+> keyword "in" <+> hsep (punctuate comma (map pretty es))
        <+> keyword "do" )
         b
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
  pretty (LuaReturn []) = keyword "return"
  pretty (LuaReturn vs) = keyword "return" <+> hsep (punctuate comma (map pretty vs))
  pretty (LuaCallS x) = pretty x

  prettyList = stmts

instance Pretty LuaVar where
  pretty (LuaName x) = text x
  pretty (LuaIndex e (LuaString k))
    | validKey k = prettyWith PreRaw e <> dot <> text k
  pretty (LuaIndex e k) = prettyWith PreRaw e <> brackets (pretty k)
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
    escape '\\' = "\\\\"
    escape x | x < ' ' = "\\" <> B.decimal (ord x)
             | otherwise = B.singleton x
  pretty (LuaNumber d) = sliteral (pretty d)
  pretty (LuaInteger d) = sliteral (pretty d)
  pretty e@(LuaBinOp l o r) =
    let prec = precedenceOf e
    in case assocOf o of
        ALeft -> prettyWith prec l <+> op o <+> prettyWith (decrPrec prec) r
        ARight -> prettyWith (decrPrec prec) l <+> op o <+> prettyWith prec r
    where
      op "and" = skeyword "and"
      op "or" = skeyword "or"
      op o = text o
  pretty e@(LuaUnOp "not" x) = skeyword "not " <> prettyWith (precedenceOf e) x
  pretty (LuaUnOp "-" x@LuaUnOp{}) = text "-" <> parens (pretty x)
  pretty e@(LuaUnOp o x) = text o <> prettyWith (precedenceOf e) x
  pretty (LuaRef x) = pretty x
  pretty (LuaFunction a b) =
    funcBlock (keyword "function" <> args (map pretty a))
              b
              (keyword "end")
  pretty (LuaTable []) = lbrace <> rbrace
  pretty (LuaTable ps) = group (contained lbrace (punctuate comma . entries 1 $ ps) rbrace) where
    entries _ [] = []
    entries n ((LuaString k, v):es) | validKey k = text k <+> value v : entries n es
    entries n ((LuaInteger k, v):es) | k == n = pretty v : entries (n + 1) es
    entries n ((k,v):es) = brackets (pretty k) <+> value v : entries n es

    value v = equals <+> pretty v
  pretty (LuaCallE x) = pretty x
  pretty (LuaQuoteE x) = "%" <> text x
  pretty (LuaBitE x) = text x

instance Pretty LuaCall where
  pretty (LuaCall x a) = prettyWith PreRaw x <> args (map pretty a)
  pretty (LuaInvoke x k a) = prettyWith PreRaw x <> ":" <> text k <> args (map pretty a)

prettyWith :: Precedence -> LuaExpr -> Doc
prettyWith desired expr =
  let actual = precedenceOf expr
      expr' = pretty expr
  in if actual > desired then parens expr' else expr'

-- | An alternative to 'block' which may group simple functions onto one line
funcBlock :: Doc -> [LuaStmt] -> Doc -> Doc
funcBlock header [] = group . block header []
funcBlock header r@[LuaReturn{}] = group . block header r
funcBlock header body = block header body

validKey :: Text -> Bool
validKey t = case T.uncons t of
               Nothing -> False
               Just (c, cs) -> start c && T.all rest cs && Set.notMember t keywords
  where
    start c = c == '_' || isAsciiUpper c || isAsciiLower c
    rest c = start c || isDigit c

-- | A set of all Lua keywords, which cannot be used as identifiers.
keywords :: Set.Set T.Text
keywords = Set.fromList
  [ "and", "break", "do", "else", "elseif", "end"
  , "false", "for" , "function",  "if", "in", "local"
  , "nil", "not", "or", "repeat", "return", "then"
  , "true", "until", "while", "goto"
  -- since these are commonly used in foreign
  -- code, we escape them to prevent clashes.
  , "os", "io", "fs", "arg", "ffi", "bit", "bitop", "next"
  , "utf8", "math", "load", "type", "table", "pcall", "love"
  , "print", "pairs", "bit32", "debug", "error", "xpcall"
  , "module", "ipairs", "assert", "rawset", "select", "string"
  , "unpack", "rawlen", "rawget", "dofile", "require"
  , "package", "loadfile", "tonumber", "tostring"
  , "rawequal", "coroutine", "loadstring", "setmetatable"
  , "getmetatable", "collectgarbage"
  ]
