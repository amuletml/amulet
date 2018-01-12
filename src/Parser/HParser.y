{
module Parser.HParser (parseInput) where

import qualified Data.Text as T

import Data.Maybe (fromJust)
import Data.Span
import Data.Spanned
import Parser.ALexer
import Parser.Token
import Syntax
import Text.Parsec.Pos (newPos)

}

%name parseInput Expr
%tokentype { Token }
%monad { Either String } { (>>=) } { return }
%error { parseError }

%token
  '->'     { Token TcArrow _ }
  '='      { Token TcEqual _ }
  '∀'      { Token TcForall _ }
  forall   { Token TcForall _ }
  '=>'     { Token TcImplies _ }
  '|'      { Token TcPipe _ }
  '**'     { Token TcDoubleStar _ }
  '*'      { Token TcStar _ }
  '+'      { Token TcAdd _ }
  '^'      { Token TcConcat _ }
  '<'      { Token TcLt _ }
  '<='     { Token TcLte _ }
  '>'      { Token TcGt _ }
  '>='     { Token TcGte _ }
  '=='     { Token TcEqEq _ }
  '&&'     { Token TcAndAnd _ }
  '||'     { Token TcOrOr _ }
  '/'      { Token TcDivide _ }
  '-'      { Token TcSubtract _ }
  '<>'     { Token TcNotEqual _ }
  '~'      { Token TcTilde _ }
  '_'      { Token TcUnderscore _ }

  let      { Token TcLet _ }
  fun      { Token TcFun _ }
  and      { Token TcAnd _ }
  if       { Token TcIf _ }
  then     { Token TcThen _ }
  else     { Token TcElse _ }
  begin    { Token TcBegin _ }
  end      { Token TcEnd _ }
  in       { Token TcIn _ }
  external { Token TcExternal _ }
  val      { Token TcVal _ }
  true     { Token TcTrue _ }
  false    { Token TcFalse _ }
  match    { Token TcMatch _ }
  with     { Token TcWith _ }
  type     { Token TcType _ }
  unit     { Token TcUnit _ }
  of       { Token TcOf _ }

  ','      { Token TcComma _ }
  ';;'     { Token TcTopSep _ }
  ';'      { Token TcSemicolon _ }
  '('      { Token TcOParen _ }
  ')'      { Token TcCParen _ }
  '{'      { Token TcOBrace _ }
  '}'      { Token TcCBrace _ }
  '['      { Token TcOSquare _ }
  ']'      { Token TcCSquare _ }

  ident    { Token (TcIdentifier _) _ }
  hole     { Token (TcHole _) _ }
  int      { Token (TcInteger _) _ }
  string   { Token (TcString  _) _ }

%%


Expr : BaseExpr                  { $1 }
     -- | Expr BaseExpr              { withPos2 $1 $2 $ App $1 $2 }

BaseExpr :: { Expr Parsed }
         : Lit                                    { withPos1 $1    $ Literal (getL $1) }
         | Var                                    { withPos1 $1    $ VarRef (getL $1) }
         | hole                                   { withPos1 $1    $ Hole (Name (getHole $1)) }
         | '(' ExprList ')'                       { withPos2 $1 $3 $ tupleExpr $2 }
         | '{' RecordList '}'                     { withPos2 $1 $3 $ Record $2 }
         | '{' Expr with RecordList '}'           { withPos2 $1 $5 $ RecordExt $2 $4 }
         | fun Pattern '->' Expr                  { withPos2 $1 $4 $ Fun $2 $4 }
         | let Var '=' Expr in Expr               { withPos2 $1 $6 $ Let [(getL $2, $4, undefined)] $6 } -- TODO: Actual
         | if Expr then Expr else Expr            { withPos2 $1 $6 $ If $2 $4 $6 }

Var :: { Located (Var Parsed) }
    : ident                { lPos1 $1 $ Name (getIdent $1) }

ExprList :: { [Expr Parsed] }
         : {- Empty -}       { [] }
         | ExprList1          { $1 }

ExprList1 :: { [Expr Parsed] }
          : Expr               { [$1] }
          | Expr ',' ExprList1  { $1 : $3 }

RecordList :: { [(T.Text, Expr p)] }
           : {- Empty -}                   { [] }
           | ident '=' Expr ',' RecordList { (getIdent $1, $3) : $5 }
           | ident '=' Expr                { [(getIdent $1, $3)] }

Lit :: { Located Lit }
    : int                  { lPos1 $1 $ LiInt (getInt $1) }
    | string               { lPos1 $1 $ LiStr (getString $1) }
    | true                 { lPos1 $1 $ LiBool True }
    | false                { lPos1 $1 $ LiBool False }

Pattern : Var              { withPos1 $1 $ Capture (getL $1) }


{

data Located a = L a Span
instance Spanned (Located a) where
  annotation (L _ s) = s

parseError :: [Token] -> Either String a
parseError x = Left ("Parse error: " ++ show x)

lPos1 :: Spanned a => a -> b -> Located b
lPos1 s x = withPos1 s (L x)

lPos2 :: (Spanned a, Spanned b) => a -> b -> c -> Located c
lPos2 s e x = withPos2 s e (L x)

withPos1 :: Spanned a => a -> (Span -> b) -> b
withPos1 s f = f (annotation s)

withPos2 :: (Spanned a, Spanned b) => a -> b -> (Span -> c) -> c
withPos2 s e f = f (mkSpanUnsafe (spanStart $ annotation s) (spanEnd $ annotation e))


tupleExpr :: [Expr Parsed] -> Ann Parsed -> Expr Parsed
tupleExpr []  a = Literal LiUnit a
tupleExpr [x] a = x
tupleExpr xs  a = Tuple xs a

getIdent  (Token (TcIdentifier x) _) = x
getHole   (Token (TcHole x) _)       = x
getInt    (Token (TcInteger x) _)    = x
getString (Token (TcString  x) _)    = x
getL      (L x _)                    = x

}
