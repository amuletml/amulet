{
{-| The parser for Lua, as defined by a Happy grammar.
-}
module Language.Lua.Parser.Parser
  ( parseExpr
  , parseStmt
  , parseStmts
  ) where

import Control.Arrow (second)

import Data.List (intercalate, nub)
import Data.Maybe (fromJust, isJust)
import qualified Data.Text as T

import Language.Lua.Parser.Wrapper
import Language.Lua.Parser.Error
import Language.Lua.Parser.Lexer
import Language.Lua.Parser.Token
import Language.Lua.Syntax

#undef __GLASGOW_HASKELL__
#define __GLASGOW_HASKELL__ 709

}

%name parseExpr Expr
%name parseStmt Stmt
%name parseStmts Stmts

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token TcEOF _ _ }
%error { parseError }
%errorhandlertype explist

%token
  and      { Token TcAnd _ _ }
  break    { Token TcBreak _ _ }
  do       { Token TcDo _ _ }
  else     { Token TcElse _ _ }
  elseif   { Token TcElseIf _ _ }
  end      { Token TcEnd _ _ }
  false    { Token TcFalse _ _ }
  for      { Token TcFor _ _ }
  function { Token TcFunction _ _ }
  if       { Token TcIf _ _ }
  in       { Token TcIn _ _ }
  local    { Token TcLocal _ _ }
  nil      { Token TcNil _ _ }
  not      { Token TcNot _ _ }
  or       { Token TcOr _ _ }
  repeat   { Token TcRepeat _ _ }
  return   { Token TcReturn _ _ }
  then     { Token TcThen _ _ }
  true     { Token TcTrue _ _ }
  until    { Token TcUntil _ _ }
  while    { Token TcWhile _ _ }

  ':'      { Token TcColon _ _ }
  ','      { Token TcComma _ _ }
  '.'      { Token TcDot _ _ }
  '...'    { Token TcDots _ _ }
  '='      { Token TcEquals _ _ }
  ';'      { Token TcSemicolon _ _ }

  '('      { Token TcOParen _ _ }
  ')'      { Token TcCParen _ _ }
  '{'      { Token TcOBrace _ _ }
  '}'      { Token TcCBrace _ _ }
  '['      { Token TcOSquare _ _ }
  ']'      { Token TcCSquare _ _ }

  '+'      { Token TcAdd _ _ }
  '-'      { Token TcSub _ _ }
  '*'      { Token TcMul _ _ }
  '/'      { Token TcDiv _ _ }
  '^'      { Token TcPow _ _ }
  '%'      { Token TcMod _ _ }

  '..'     { Token TcConcat _ _ }

  '=='     { Token TCEq _ _ }
  '~='     { Token TCNe _ _ }
  '<'      { Token TcLt _ _ }
  '>'      { Token TcGt _ _ }
  '<='     { Token TcLe _ _ }
  '>='     { Token TcGe _ _ }

  '#'      { Token TcLen _ _ }

  ident    { Token (TcIdentifier _) _ _ }

  qexpr    { Token (TcQuoteE _) _ _ }
  qstmt    { Token (TcQuoteS _) _ _ }
  qvar     { Token (TcQuoteV _) _ _ }

  int      { Token (TcInteger _) _ _ }
  float    { Token (TcFloat _) _ _ }
  string   { Token (TcString  _) _ _ }

%right '^'
%right UNOP
%left '*' '/' '%'
%left '+' '-'
%right '..'
%left '<' '>' '<=' '>=' '~=' '=='
%left and
%left or
%%

Ident :: { LuaVar }
  : ident                               { LuaName (getIdent $1) }
  | qvar                                { LuaQuoteV (getIdent $1) }

Var :: { LuaVar }
  : Ident                               { $1 }
  | BaseExpr '.' ident                  { LuaIndex $1 (LuaString . getIdent $ $3) }
  | BaseExpr '[' Expr ']'               { LuaIndex $1 $3 }

BaseExpr :: { LuaExpr }
  : Var                                 { LuaRef $1 }
  | qexpr                               { LuaQuoteE (getIdent $1) }
  | '(' Expr ')'                        { $2 }
  | Call                                { LuaCallE $1 }

Call :: { LuaCall }
  : BaseExpr Args                       { LuaCall $1 $2 }
  | BaseExpr ':' ident Args             { LuaInvoke $1 (getIdent $3) $4 }

Args :: { [LuaExpr] }
  : '(' List(Expr, ',') ')'             { $2 }
  | string                              { [LuaString (getString $1)] }
  | Table                               { [$1] }

Atom :: { LuaExpr }
  : BaseExpr                            { $1 }
  | Table                               { $1 }

  | nil                                 { LuaNil }
  | true                                { LuaTrue }
  | false                               { LuaFalse }
  | '...'                               { LuaDots }

  | int                                 { LuaInteger (getInt $1) }
  | float                               { LuaNumber (getFloat  $1) }
  | string                              { LuaString (getString $1) }
  | function '(' List(Ident, ',') ')' Stmts end { LuaFunction  $3 $5 }

Expr :: { LuaExpr }
  : Atom                                { $1 }

  | Expr and Expr                       { LuaBinOp $1 (T.pack "and") $3 }
  | Expr or  Expr                       { LuaBinOp $1 (T.pack "or")  $3 }

  | Expr '+' Expr                       { LuaBinOp $1 (T.pack "+") $3 }
  | Expr '-' Expr                       { LuaBinOp $1 (T.pack "-") $3 }
  | Expr '*' Expr                       { LuaBinOp $1 (T.pack "*") $3 }
  | Expr '/' Expr                       { LuaBinOp $1 (T.pack "/") $3 }
  | Expr '^' Expr                       { LuaBinOp $1 (T.pack "^") $3 }
  | Expr '%' Expr                       { LuaBinOp $1 (T.pack "%") $3 }

  | Expr '..' Expr                      { LuaBinOp $1 (T.pack "..") $3 }

  | Expr '==' Expr                      { LuaBinOp $1 (T.pack "==") $3 }
  | Expr '~=' Expr                      { LuaBinOp $1 (T.pack "~=") $3 }
  | Expr '<'  Expr                      { LuaBinOp $1 (T.pack "<")  $3 }
  | Expr '>'  Expr                      { LuaBinOp $1 (T.pack ">")  $3 }
  | Expr '<=' Expr                      { LuaBinOp $1 (T.pack "<=") $3 }
  | Expr '>=' Expr                      { LuaBinOp $1 (T.pack ">=") $3 }

  | '-' Expr %prec UNOP                 { LuaUnOp (T.pack "-")      $2 }
  | '#' Expr %prec UNOP                 { LuaUnOp (T.pack "#")      $2 }
  | not Expr %prec UNOP                 { LuaUnOp (T.pack "not")    $2 }

Table :: { LuaExpr }
  : '{' List(TablePair, ',') '}'        { LuaTable (buildTable $2) }

TablePair :: { Either LuaExpr (LuaExpr, LuaExpr) }
  : Expr                                { Left $1 }
  | ident '=' Expr                      { Right (LuaString (getIdent $1), $3) }
  | '[' Expr ']' '=' Expr               { Right ($2, $5) }

Stmts :: { [LuaStmt] }
  :                                     { [] }
  | ';' Stmts                           { $2 }
  | Stmt Stmts                          { $1 : $2 }

Stmt :: { LuaStmt }
  : qstmt                               { LuaQuoteS (getIdent $1) }
  | Call                                { LuaCallS $1 }

  | do Stmts end                        { LuaDo $2 }
  | List1(Var, ',') '=' List1(Expr, ',') { LuaAssign $1 $3 }
  | while Expr do Stmts end             { LuaWhile $2 $4 }
  | repeat Stmts until Expr             { LuaRepeat $2 $4 }
  | if Expr then Stmts ElseIfs end      { LuaIfElse (($2,$4):$5) }
  | for Ident '=' Expr ',' Expr do Stmts end          { LuaFornum $2 $4 $6 (LuaInteger 1) $8 }
  | for Ident '=' Expr ',' Expr ',' Expr do Stmts end { LuaFornum $2 $4 $6 $8 $10 }
  | for List1(Ident, ',') in List1(Expr, ',') do Stmts end { LuaFor $2 $4 $6 }
  | local List1(Ident, ',') '=' List1(Expr, ',') { LuaLocal $2 $4 }
  | local function Ident '(' List(Ident, ',') ')' Stmts end { LuaLocalFun $3 $5 $7 }
  | function Ident '(' List(Ident, ',') ')' Stmts end { LuaAssign [$2] [LuaFunction $4 $6] }
  | return List1(Expr, ',')             { LuaReturn $2 }
  | break                               { LuaBreak }

ElseIfs :: { [(LuaExpr, [LuaStmt])] }
  :                                     { [] }
  | else Stmts                          { [(LuaTrue, $2)] }
  | elseif Expr then Stmts ElseIfs      { ($2, $4):$5 }

List(p, s)
    : {- Empty -}       { [] }
    | List1(p, s)       { $1 }

List1(p, s)
     : p                { [$1] }
     | p s List1(p, s)  { $1 : $3 }

{

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

parseError :: (Token, [String]) -> Parser a
parseError (tok, exp) = failWith mainErr

  where
    mainErr = UnexpectedToken tok (nub (map unmap exp))

    unmap :: String -> String
    unmap "ident" = "identifier"
    unmap "qexpr" = "quasi-quoted expression"
    unmap "qstmt" = "quasi-quoted statement"
    unmap "qvar"  = "quasi-quoted variable"
    unmap x = x

buildTable = go 1 where
  go _ [] = []
  go n (Left x:xs) = (LuaInteger n, x):go (n + 1) xs
  go n (Right x:xs) = x:go n xs

getIdent (Token (TcIdentifier x) _ _) = x
getIdent (Token (TcQuoteE x) _ _) = x
getIdent (Token (TcQuoteS x) _ _) = x
getIdent (Token (TcQuoteV x) _ _) = x

getInt    (Token (TcInteger x) _ _)    = x
getFloat  (Token (TcFloat x) _ _)      = x
getString (Token (TcString  x) _ _)    = x

}
