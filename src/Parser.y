{
module Parser (parseInput) where

import Control.Arrow (second)

import Data.List (intercalate)
import Data.Maybe (fromJust, isJust)
import Data.Span
import Data.Spanned
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import Parser.Lexer
import Parser.Wrapper
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
  '*'      { Token TcStar _ }
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
  function { Token TcFunction _ }
  type     { Token TcType _ }
  of       { Token TcOf _ }
  module   { Token TcModule _ }
  open     { Token TcOpen _ }
  as       { Token TcAs _ }

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

  op       { Token (TcOp _) _ }
  ident    { Token (TcIdentifier _) _ }
  opid     { Token (TcOpIdent _) _ }
  conid    { Token (TcConIdent _) _ }
  qident   { Token (TcIdentifierQual _ _) _ }
  qopid    { Token (TcOpIdentQual _ _) _ }
  qconid   { Token (TcConIdentQual _ _) _ }
  qdotid   { Token (TcDotQual _) _ }
  access   { Token (TcAccess _) _ }
  tyvar    { Token (TcTyVar _) _ }
  hole     { Token (TcHole _) _ }
  int      { Token (TcInteger _) _ }
  float    { Token (TcFloat _) _ }
  string   { Token (TcString  _) _ }

%expect 74

%%

Tops :: { [Toplevel Parsed] }
     : List1(Top, ';;')                        { $1 }

Top :: { Toplevel Parsed }
    : let BindGroup                             { LetStmt (reverse $2) }
    | fun FuncGroup                             { FunStmt (reverse $2) }
    | external val ident ':' Type '=' string    { withPos2 $1 $7 $ ForeignVal (getName $3) (getString $7) (getL $5) }

    | type ident ListE(TyVar)                          { TypeDecl (getName $2) (map getL $3) [] }
    | type ident ListE(TyVar) '=' List1(Ctor, '|')     { TypeDecl (getName $2) (map getL $3) $5 }
    | type ident ListE(TyVar) '=' '|' List1(Ctor, '|') { TypeDecl (getName $2) (map getL $3) $6 }


    | module qconid '=' begin Tops end         { Module (getName $2) $5 }
    | module conid '=' begin Tops end          { Module (getName $2) $5 }
    | module conid '=' Con                     { Open (getL $4) (Just (getIdent $2)) }
    | open Con                                 { Open (getL $2) Nothing }

Ctor :: { Constructor Parsed }
     : conid                                   { withPos1 $1 $ UnitCon (getName $1) }
     | conid of Type                           { withPos2 $1 $3 $ ArgCon (getName $1) (getL $3) }
     | conid ':' Type                          { withPos2 $1 $3 $ GeneralisedCon (getName $1) (getL $3) }

Expr :: { Expr Parsed }
     : ExprApp                                 { $1 }
     | Expr Operator ExprApp                   { withPos2 $1 $3 $ BinOp $1 $2 $3 }

ExprApp :: { Expr Parsed }
        : Expr0                                { $1 }
        | ExprApp Atom                         { withPos2 $1 $2 $ App $1 $2 }
        | ExprApp ':' Type                     { withPos2 $1 $3 $ Ascription $1 (getL $3) }

Expr0 :: { Expr Parsed }
      : fun ListE1(ArgP) '->' Expr             { foldr (\x y -> withPos2 x $4 $ Fun x y) $4 $2 }
      | let BindGroup in Expr                  { withPos2 $1 $4 $ Let (reverse $2) $4 }
      | if Expr then Expr else Expr            { withPos2 $1 $6 $ If $2 $4 $6 }
      | match List1(Expr, ',') with ListE1(Arm)
        { withPos2 $1 $3 $ Match (completeTuple Tuple $2) $4 }
      | function ListE1(Arm)                   { withPos1 $1 $ Function $2 }
      | Atom                                   { $1 }

Atom :: { Expr Parsed }
     : Var                                    { withPos1 $1 (VarRef (getL $1)) }
     | Con                                    { withPos1 $1 (VarRef (getL $1)) }
     | Lit                                    { withPos1 $1 (Literal (getL $1)) }
     | hole                                   { withPos1 $1 (Hole (Name (getHole $1))) }
     | '_'                                    { withPos1 $1 (Hole (Name (T.singleton '_'))) }
     | begin List1(Expr, ';') end             { withPos2 $1 $3 $ Begin $2 }
     | qdotid Atom                            { withPos2 $1 $2 $ OpenIn (getName $1) $2 }
     | '(' ')'                                { withPos2 $1 $2 $ Literal LiUnit }
     | '(' Section ')'                        { $2 }
     | '(' List1(NullSection, ',') ')'        { withPos2 $1 $3 $ tupleExpr $2 }
     | '{' Rows('=', Expr) '}'                { withPos2 $1 $3 $ Record $2 }
     | '{' Expr with Rows('=',Expr) '}'       { withPos2 $1 $5 $ RecordExt $2 $4 }

     | Atom access                            { withPos2 $1 $2 $ Access $1 (getIdent $2) }

Operator :: { Expr Parsed }
         : '*'                                { withPos1 $1 $ varE "*" }
         | '~'                                { withPos1 $1 $ varE "~" }
         | op                                 { withPos1 $1 $ VarRef (getName $1) }
         | opid                               { withPos1 $1 $ VarRef (getName $1) }
         | qopid                              { withPos1 $1 $ VarRef (getName $1) }

Section :: { Expr Parsed }
        : Expr                                { $1 }
        | access                              { withPos1 $1 $ AccessSection (getIdent $1) }
        | Operator                            { withPos1 $1 $ BothSection $1 }
        | Expr Operator                       { withPos2 $1 $2 $ RightSection $1 $2 }
        | Operator Expr                       { withPos2 $1 $2 $ LeftSection $1 $2 }

NullSection :: { Maybe (Expr Parsed) }
  :                                           { Nothing }
  | Section                                   { Just $1 }

Var :: { Located (Var Parsed) }
    : ident  { lPos1 $1 $ getName $1 }
    | qident { lPos1 $1 $ getName $1 }

Con :: { Located (Var Parsed) }
    : conid      { lPos1 $1 $ getName $1 }
    | qconid     { lPos1 $1 $ getName $1 }

TyVar :: { Located (Var Parsed) }
      : tyvar { lPos1 $1 $ Name (getIdent $1) }

BindGroup :: { [(Var Parsed, Expr Parsed, Ann Parsed)] }
          : Binding                           { [$1] }
          | BindGroup and Binding             { $3 : $1 }

Binding :: { (Var Parsed, Expr Parsed, Ann Parsed) }
        : BindName ListE(ArgP) '=' Expr
          { (getL $1, foldr (\x y -> withPos2 x $4 (Fun x y)) $4 $2, withPos2 $1 $4 id) }
        | BindName ListE(ArgP) ':' Type '=' Expr
          { (getL $1, (foldr (\x y -> withPos2 x $6 (Fun x y)) (Ascription $6 (getL $4) (withPos2 $1 $6 id)) $2), withPos2 $1 $6 id) }

FuncGroup :: { [Function Parsed] }
          : FunctionDecl               { [$1] }
          | FuncGroup and FunctionDecl { $3 : $1 }

FunctionDecl :: { Function Parsed }
             : BindName ':' Type ListE1(Clause) { withPos2 $1 (foldr1 (<>) (map annotation $4)) (FunDecl (getL $1) $4 (getL $3)) }

Clause :: { Clause Parsed }
       : '|' BindName ListE1(ArgP) '=' Expr { withPos2 $1 $5 $ Clause (getL $2) $3 $5 }

BindName :: { Located (Var Parsed) }
     : ident                                   { lPos1 $1 $ getName $1 }
     | '(' op ')'                              { lPos2 $1 $3 $ getName $2 }

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
    | float                { lPos1 $1 $ LiFloat (getFloat $1) }
    | string               { lPos1 $1 $ LiStr (getString $1) }
    | true                 { lPos1 $1 $ LiBool True }
    | false                { lPos1 $1 $ LiBool False }

Pattern :: { Pattern Parsed }
        : ArgP             { $1 }
        | Con Pattern      { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
        | ArgP ':' Type    { withPos2 $1 $3 $ PType $1 (getL $3) }

ArgP :: { Pattern Parsed }
     : ident                      { withPos1 $1 $ Capture (getName $1) }
     | '_'                        { withPos1 $1 $ Wildcard }
     | Con                        { withPos1 $1 $ Destructure (getL $1) Nothing }
     | '{' Rows('=',Pattern) '}'  { withPos2 $1 $3 $ PRecord $2 }
     | '(' List(Pattern, ',') ')' { withPos2 $1 $3 $ tuplePattern $2 }
     | Lit                        { withPos1 $1 (PLiteral (getL $1)) }

Arm :: { (Pattern Parsed, Expr Parsed) }
    : '|' List1(Pattern, ',') '->' Expr       { (completeTuple PTuple $2, $4) }


Type :: { Located (Type Parsed) }
     : TypeProd                                   { $1 }
     | TypeProd '->' Type                         { lPos2 $1 $3 $ TyPi (Anon (getL $1)) (getL $3) }

TypeProd :: { Located (Type Parsed) }
         : TypeApp                                { $1 }
         | TypeApp '*' TypeApp                    { lPos2 $1 $3 $ TyTuple (getL $1) (getL $3) }

TypeApp  :: { Located (Type Parsed) }
         : TypeAtom                               { $1 }
         | TypeApp TypeAtom                       { lPos2 $1 $2 $ TyApp (getL $1) (getL $2) }

TypeAtom :: { Located (Type Parsed) }
         : Var                                    { lPos1 $1 $ TyCon (getL $1) }
         | TyVar                                  { lPos1 $1 $ TyVar (getL $1) }
         | Con
           { lPos1 $1 $ TyPromotedCon (getL $1) }
         | forall ListE1(tyvar) '.' Type
           { lPos2 $1 $4 $ forallTy (map getName $2) (getL $4) }
         | '(' ')'                                { lPos2 $1 $2 $ TyCon (Name (T.pack "unit")) }
         | '(' Type ')'                           { lPos2 $1 $3 (getL $2) }
         | '{' Rows(':', Type) '}'                { lPos2 $1 $3 $ TyExactRows (map (second getL) $2) }
         | '{' Type '|' Rows(':', Type) '}'       { lPos2 $1 $5 $ TyRows (getL $2) (map (second getL) $4) }
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

tupleExpr :: [Maybe (Expr Parsed)] -> Ann Parsed -> Expr Parsed
tupleExpr xs | all isJust xs = Tuple (map fromJust xs)
             | otherwise = TupleSection xs

completeTuple :: Spanned (f Parsed) => ([f Parsed] -> Span -> f Parsed) -> [f Parsed] -> f Parsed
completeTuple _ [x] = x
completeTuple k (x:xs) = k (x:xs) (sconcat (annotation x :| map annotation xs))

tuplePattern :: [Pattern Parsed] -> Ann Parsed -> Pattern Parsed
tuplePattern [] a = PLiteral LiUnit a
tuplePattern [x] a = case x of
  PType x t _ -> PType x t a
  _ -> x
tuplePattern xs a = PTuple xs a

varE = VarRef . Name . T.pack

getIdent  (Token (TcOp x) _)         = x
getIdent  (Token (TcIdentifier x) _) = x
getIdent  (Token (TcOpIdent x) _)    = x
getIdent  (Token (TcConIdent x) _)   = x
getIdent  (Token (TcAccess x) _)     = x
getIdent  (Token (TcTyVar x) _)      = x

getName (Token (TcOp x) _)                = Name x
getName (Token (TcIdentifier x) _)        = Name x
getName (Token (TcOpIdent x) _)           = Name x
getName (Token (TcConIdent x) _)          = Name x
getName (Token (TcIdentifierQual ms x) _) = foldl (flip InModule) (Name x) ms
getName (Token (TcOpIdentQual ms x) _)    = foldl (flip InModule) (Name x) ms
getName (Token (TcConIdentQual ms x) _)   = foldl (flip InModule) (Name x) ms
getName (Token (TcDotQual ms) _)          = foldl (flip InModule) (Name (last ms)) (init ms)
getName (Token (TcTyVar x) _)             = Name x

getHole   (Token (TcHole x) _)       = x
getInt    (Token (TcInteger x) _)    = x
getFloat  (Token (TcFloat x) _)      = x
getString (Token (TcString  x) _)    = x
getL      (L x _)                    = x

forallTy vs t = foldr TyPi t (map (flip Implicit Nothing) vs)
}
