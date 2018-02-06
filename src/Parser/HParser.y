{
module Parser.HParser (parseInput) where

import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Span
import Data.Spanned
import qualified Data.Text as T

import Parser.ALexer
import Parser.AWrapper
import Parser.Token
import Syntax

}

%name parseInput Tops
%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token TcEOF _ }
%error { parseError }
%errorhandlertype explist

%token
  '->'     { Token TcArrow _ }
  '='      { Token TcEqual _ }
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
  '.'      { Token TcDot _ }
  ':'      { Token TcColon _ }
  ';;'     { Token TcTopSep _ }
  ';'      { Token TcSemicolon _ }
  '('      { Token TcOParen _ }
  ')'      { Token TcCParen _ }
  '{'      { Token TcOBrace _ }
  '}'      { Token TcCBrace _ }
  '['      { Token TcOSquare _ }
  ']'      { Token TcCSquare _ }

  ident    { Token (TcIdentifier _) _ }
  conid    { Token (TcConIdent _) _ }
  tyvar    { Token (TcTyVar _) _ }
  hole     { Token (TcHole _) _ }
  int      { Token (TcInteger _) _ }
  string   { Token (TcString  _) _ }

%%

Tops :: { [Toplevel Parsed] }
     : List1(Top, ';;')                        { $1 }

Top :: { Toplevel Parsed }
    : let BindGroup                            { LetStmt $2 }
    | external val Var ':' Type '=' string     { withPos2 $1 $7 $ ForeignVal (getL $3) (getString $7) $5 }

    | type Var ListE(TyVar)                          { TypeDecl (getL $2) $3 [] }
    | type Var ListE(TyVar) '=' List1(Ctor, '|')     { TypeDecl (getL $2) $3 $5 }
    | type Var ListE(TyVar) '=' '|' List1(Ctor, '|') { TypeDecl (getL $2) $3 $6 }

Ctor :: { Constructor Parsed }
     : conid                                   { withPos1 $1 $ UnitCon (Name (getIdent $1)) }
     | conid of Type                           { withPos2 $1 $2 $ ArgCon (Name (getIdent $1)) $3 }

Expr : Expr0     { $1 }
     | Expr Atom { withPos2 $1 $2 $ App $1 $2 }

Expr0 :: { Expr Parsed }
      : fun ArgP '->' Expr                     { withPos2 $1 $4 $ Fun $2 $4 }
      | let BindGroup in Expr                  { withPos2 $1 $4 $ Let $2 $4 }
      | if Expr then Expr else Expr            { withPos2 $1 $6 $ If $2 $4 $6 }
      | match Expr with ListE1(Arm)            { withPos2 $1 $3 $ Match $2 $4 }
      | Atom                                   { $1 }

Atom :: { Expr Parsed }
     : Var  { withPos1 $1 (VarRef  (getL $1)) }
     | Lit  { withPos1 $1 (Literal (getL $1)) }
     | hole { withPos1 $1 (Hole (Name (getHole $1))) }
     | begin List1(Expr, ';') end             { withPos2 $1 $3 $ Begin $2 }
     | '(' List(Expr, ',') ')'                { withPos2 $1 $3 $ tupleExpr $2 }
     | '{' Rows('=', Expr) '}'                { withPos2 $1 $3 $ Record $2 }
     | '{' Expr with Rows('=',Expr) '}'       { withPos2 $1 $5 $ RecordExt $2 $4 }
     | Atom ':' Type                          { withPos2 $1 $2 $ Ascription $1 $3 }

Var :: { Located (Var Parsed) }
    : ident { lPos1 $1 $ Name (getIdent $1) }

Con :: { Located (Var Parsed) }
    : conid { lPos1 $1 $ Name (getIdent $1) }

TyVar :: { Var Parsed }
      : tyvar { Name (T.tail (getIdent $1)) }

BindGroup :: { [(Var Parsed, Expr Parsed, Ann Parsed)] }
          : Binding { [$1] }
          | BindGroup and Binding { $3 : $1 }

Binding :: { (Var Parsed, Expr Parsed, Ann Parsed) }
        : Var '=' Expr { (getL $1, $3, withPos2 $1 $3 id) }

List(p, s)
    : {- Empty -}       { [] }
    | List1(p, s)       { $1 }

List1(p, s)
     : p                { [$1] }
     | p s List1(p, s)  { $1 : $3 }

ListE(p)
     : {- Empty -}      { [] }
     | ListE1(p)        { $1 }

ListE1(p)
     : p                { [$1] }
     | p ListE1(p)      { $1 : $2 }

Rows(p, q)
   : {- Empty -}             { [] }
   | ident p q ',' Rows(p,q) { (getIdent $1, $3) : $5 }
   | ident p q               { [(getIdent $1, $3)] }

Lit :: { Located Lit }
    : int                  { lPos1 $1 $ LiInt (getInt $1) }
    | string               { lPos1 $1 $ LiStr (getString $1) }
    | true                 { lPos1 $1 $ LiBool True }
    | false                { lPos1 $1 $ LiBool False }

Pattern :: { Pattern Parsed }
        : ArgP           { $1 }
        | Con Pattern    { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }

ArgP :: { Pattern Parsed }
     : Var                        { withPos1 $1 $ Capture (getL $1) }
     | Con                        { withPos1 $1 $ Destructure (getL $1) Nothing }
     | '{' Rows('=',Pattern) '}'  { withPos2 $1 $3 $ PRecord $2 }
     | '(' List(Pattern, ',') ')' { withPos2 $1 $3 $ tuplePattern $2 }

Arm :: { (Pattern Parsed, Expr Parsed) }
    : '|' Pattern '->' Expr       { ($2, $4) }


Type :: { Type Parsed }
     : TypeProd                                   { $1 }
     | TypeProd '->' Type                         { TyArr $1 $3 }

TypeProd :: { Type Parsed }
         : TypeApp                                { $1 }
         | TypeApp '*' TypeApp                    { TyTuple $1 $3 }

TypeApp  :: { Type Parsed }
         : TypeAtom                               { $1 }
         | TypeApp TypeAtom                       { TyApp $1 $2 }

TypeAtom :: { Type Parsed }
         : Var                                    { TyCon (getL $1) }
         | TyVar                                  { TyVar $1 }
         | forall ListE1(tyvar) '.' Type          { TyForall (map (Name . getIdent) $2) $4 }
         | '(' ')'                                { TyCon (Name (T.pack "unit")) }
         | '(' Type ')'                           { $2 }
         | '{' Rows(':', Type) '}'                { TyExactRows $2 }
         | '{' Type '|' Rows(':', Type) '}'       { TyRows $2 $4 }
{

data Located a = L a Span

instance Spanned (Located a) where
  annotation (L _ s) = s

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerScan >>=)

parseError :: (Token, [String]) -> Parser a
parseError (Token s p, [])  = failPos ("Unexpected " ++ show s) p
parseError (Token s p, [x]) = failPos ("Unexpected " ++ show s ++ ", expected " ++ x) p
parseError (Token s p, xs)  = failPos ("Unexpected " ++ show s ++ ", expected one of " ++ intercalate ", " xs) p

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

tuplePattern :: [Pattern Parsed] -> Ann Parsed -> Pattern Parsed
tuplePattern [x] a = x
tuplePattern xs a = PTuple xs a

getIdent  (Token (TcIdentifier x) _) = x
getIdent  (Token (TcConIdent x) _) = x
getIdent  (Token (TcTyVar x) _) = x
getHole   (Token (TcHole x) _)       = x
getInt    (Token (TcInteger x) _)    = x
getString (Token (TcString  x) _)    = x
getL      (L x _)                    = x
}
