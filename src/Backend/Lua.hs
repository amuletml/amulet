module Backend.Lua where

import Pretty

data LuaStmt
  = LuaDo [LuaStmt]
  | LuaAssign [LuaVar] [LuaExpr]
  | LuaWhile LuaExpr [LuaStmt]
  | LuaRepeat [LuaStmt] LuaExpr
  | LuaIf LuaExpr [LuaStmt] [LuaStmt]
  | LuaFornum String LuaExpr LuaExpr LuaExpr [LuaStmt]
  | LuaLocal [LuaVar] [LuaExpr]
  | LuaReturn LuaExpr
  | LuaBreak
  | LuaCallS LuaExpr [LuaExpr]
  | LuaBit String
  deriving (Eq, Show, Ord)

data LuaVar
  = LuaName String
  | LuaIndex LuaExpr LuaExpr
  deriving (Eq, Show, Ord)

data LuaExpr
  = LuaCall LuaExpr [LuaExpr]
  | LuaNil | LuaTrue | LuaFalse | LuaDots
  | LuaRef LuaVar
  | LuaNumber Double
  | LuaString String
  | LuaFunction [LuaVar] [LuaStmt]
  | LuaTable [(LuaExpr, LuaExpr)]
  | LuaBinOp LuaExpr String LuaExpr
  | LuaBitE String
  deriving (Eq, Show, Ord)

instance Pretty LuaStmt where
  pprint (LuaDo xs) = do
    kwClr "do"
    body 2 xs *> newline
    kwClr "end"
  pprint (LuaAssign ns xs) = interleave ", " ns <+> opClr " = " <+> interleave ", " xs
  pprint (LuaWhile c t) = do
    kwClr "while " <+> c <+> kwClr " do"
    body 2 t *> newline
    kwClr "end"
  pprint (LuaRepeat t c) = do
    kwClr "repeat"
    body 2 t *> newline
    kwClr "until " <+> c
  pprint (LuaIf c t []) = do
    kwClr "if " <+> c <+> kwClr " then"
    body 2 t
    kwClr "end"
  pprint (LuaIf c t e) = do
    kwClr "if " <+> c <+> kwClr " then"
    body 2 t *> newline
    kwClr "else"
    body 2 e *> newline
    kwClr "end"
  pprint (LuaFornum v s e i b) = do
    kwClr "for " <+> v <+> opClr " = "
    interleave ", " [s, e, i]
    kwClr " do "
    body 2 b *> newline
    kwClr "end"
  pprint (LuaLocal [n] [LuaFunction a b]) = do
    kwClr "local function " <+> n <+> parens (interleave ", " a)
    body 2 b *> newline
    kwClr "end"
  pprint (LuaLocal vs []) = do
    kwClr "local "
    interleave ", " vs
  pprint (LuaLocal vs xs) = do
    kwClr "local "
    interleave ", " vs
    opClr " = "
    interleave ", " xs
  pprint (LuaBit x) = tell x
  pprint LuaBreak = kwClr "break"
  pprint (LuaReturn v) = kwClr "return " <+> v
  pprint (LuaCallS x@LuaFunction{} a) = parens x <+> parens (interleave ", " a) <+> ";"
  pprint (LuaCallS x a) = x <+> parens (interleave ", " a)

instance Pretty LuaVar where
  pprint (LuaName x) = pprint x
  pprint (LuaIndex e k) = e <+> squares k

instance Pretty LuaExpr where
  pprint LuaTrue = kwClr "true"
  pprint LuaFalse = kwClr "false"
  pprint LuaDots = opClr "..."
  pprint LuaNil = litClr "nil"
  pprint (LuaString k) = str k
  pprint (LuaNumber d) = litClr d
  pprint (LuaBinOp l o r) = l <+> " " <+> opClr o <+> " " <+> opClr r
  pprint (LuaRef x) = pprint x
  pprint (LuaFunction a b) = do
    kwClr "function " <+> parens (interleave ", " a)
    body 2 b *> newline
    kwClr "end"
  pprint (LuaTable ps) = braces $ do
    forM_ ps $ \(k, v) -> squares k <+> opClr " = " <+> v <+> ", "
  pprint (LuaCall x@LuaFunction{} a) = parens x <+> parens (interleave ", " a)
  pprint (LuaCall x a) = x <+> parens (interleave ", " a)
  pprint (LuaBitE x) = tell x
