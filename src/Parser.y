{
module Parser
  ( parseTops
  , parseRepl
  ) where

import Control.Arrow (second)

import Data.List (intercalate, nub)
import Data.Maybe (fromJust, isJust)
import Data.Span
import Data.Spanned
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import Parser.Wrapper
import Parser.Context
import Parser.Error
import Parser.Lexer
import Parser.Token

import Syntax.Var
import Syntax

}

%name parseTops Tops
%name parseRepl Repl

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token TcEOF _ _ }
%error { parseError }
%errorhandlertype explist

%token
  '->'     { Token TcArrow _ _ }
  '='      { Token TcEqual _ _ }
  forall   { Token TcForall _ _ }
  '=>'     { Token TcImplies _ _ }
  '|'      { Token TcPipe _ _ }
  '*'      { Token TcStar _ _ }
  '~'      { Token TcTilde _ _ }
  '_'      { Token TcUnderscore _ _ }

  let      { Token TcLet _ _ }
  fun      { Token TcFun _ _ }
  and      { Token TcAnd _ _ }
  if       { Token TcIf _ _ }
  then     { Token TcThen _ _ }
  else     { Token TcElse _ _ }
  begin    { Token TcBegin _ _ }
  end      { Token TcEnd _ _ }
  in       { Token TcIn _ _ }
  external { Token TcExternal _ _ }
  implicit { Token TcImplicit _ _ }
  val      { Token TcVal _ _ }
  true     { Token TcTrue _ _ }
  false    { Token TcFalse _ _ }
  match    { Token TcMatch _ _ }
  with     { Token TcWith _ _ }
  function { Token TcFunction _ _ }
  type     { Token TcType _ _ }
  of       { Token TcOf _ _ }
  module   { Token TcModule _ _ }
  open     { Token TcOpen _ _ }
  lazy     { Token TcLazy _ _ }

  ','      { Token TcComma _ _ }
  '.'      { Token TcDot _ _ }
  ':'      { Token TcColon _ _ }
  ';;'     { Token TcTopSep _ _ }
  ';'      { Token TcSemicolon _ _ }
  '('      { Token TcOParen _ _ }
  '?('     { Token TcQParen _ _ }
  ')'      { Token TcCParen _ _ }
  '@{'     { Token TcAtBrace _ _ }
  '?'      { Token TcQuestion _ _ }
  '{'      { Token TcOBrace _ _ }
  '}'      { Token TcCBrace _ _ }
  '['      { Token TcOSquare _ _ }
  ']'      { Token TcCSquare _ _ }

  op       { Token (TcOp _) _ _ }
  ident    { Token (TcIdentifier _) _ _ }
  iident   { Token (TcQIdentifier _) _ _ }
  opid     { Token (TcOpIdent _) _ _ }
  conid    { Token (TcConIdent _) _ _ }
  qident   { Token (TcIdentifierQual _ _) _ _ }
  qopid    { Token (TcOpIdentQual _ _) _ _ }
  qconid   { Token (TcConIdentQual _ _) _ _ }
  qdotid   { Token (TcDotQual _) _ _ }
  access   { Token (TcAccess _) _ _ }
  tyvar    { Token (TcTyvar _) _ _ }
  hole     { Token (TcHole _) _ _ }
  int      { Token (TcInteger _) _ _ }
  float    { Token (TcFloat _) _ _ }
  string   { Token (TcString  _) _ _ }

  '$begin' { Token TcVBegin _ _ }
  '$end'   { Token TcVEnd _ _ }
  '$in'    { Token TcVIn _ _ }
  '$sep'   { Token TcVSep _ _ }


%expect 8

%%

Tops :: { [Toplevel Parsed] }
     : List1(Top, TopSep)                      { $1 }

TopSep :: { () }
    : ';;'   { () }
    | '$sep' { () }

Top :: { Toplevel Parsed }
    : let BindGroup                             { LetStmt (reverse $2) }
    | external val BindName ':' Type '=' string    { withPos2 $1 $7 $ ForeignVal (getL $3) (getString $7) (getL $5) }

    | type ident ListE(TyConArg)                          { TypeDecl (getName $2) $3 [] }
    | type ident ListE(TyConArg) '=' List1(Ctor, '|')     { TypeDecl (getName $2) $3 $5 }
    | type ident ListE(TyConArg) '=' '|' List1(Ctor, '|') { TypeDecl (getName $2) $3 $6 }


    | module qconid '=' begin Tops end         { Module (getName $2) $5 }
    | module conid '=' begin Tops end          { Module (getName $2) $5 }
    | module conid '=' '$begin' Tops '$end'    { Module (getName $2) $5 }
    | module conid '=' Con                     { Open (getL $4) (Just (getIdent $2)) }
    | open Con                                 { Open (getL $2) Nothing }

TyConArg :: { TyConArg Parsed }
         : TyVar { TyVarArg (getL $1) }
         | '(' TyVar ':' Type ')' { TyAnnArg (getL $2) (getL $4) }

Ctor :: { Constructor Parsed }
     : conid                                   { withPos1 $1 $ UnitCon (getName $1) }
     | conid of Type                           { withPos2 $1 $3 $ ArgCon (getName $1) (getL $3) }
     | conid ':' Type                          { withPos2 $1 $3 $ GeneralisedCon (getName $1) (getL $3) }

Repl :: { Either (Toplevel Parsed) (Expr Parsed) }
     : Top   ReplSep                           { Left $1 }
     | Expr  ReplSep                           { Right $1 }

ReplSep :: { () }
    : ';;'   { () }
    | '$sep' { () }
    |        { () }

Expr :: { Expr Parsed }
     : ExprApp                                 { $1 }
     | Expr Operator ExprApp                   { withPos2 $1 $3 $ BinOp $1 $2 $3 }

ExprApp :: { Expr Parsed }
        : Expr0                                { $1 }
        | ExprApp Atom                         { withPos2 $1 $2 $ App $1 $2 }
        | ExprApp ':' Type                     { withPos2 $1 $3 $ Ascription $1 (getL $3) }

Expr0 :: { Expr Parsed }
      : fun ListE1(Parameter) '->' ExprBlock '$end' { respanFun $1 $4 $ foldr (\x y -> withPos2 x $4 $ Fun x y) $4 $2 }
      | let BindGroup ExprIn ExprBlock '$end'  { withPos2 $1 $4 $ Let (reverse $2) $4 }
      | let open Con ExprIn ExprBlock '$end'   { withPos2 $1 $5 $ OpenIn (getL $3) $5 }
      | if Expr then ExprBlock else ExprBlock '$end'
          { withPos2 $1 $6 $ If $2 $4 $6 }
      | match List1(Expr, ',') with ListE1(Arm) '$end'
          { withPos2 $1 $3 $ Match (completeTuple Tuple $2) $4 }
      | match List1(Expr, ',') with '(' ')'
          { withPos2 $1 $3 $ Match (completeTuple Tuple $2) [] }
      | function ListE1(Arm) '$end'            { withPos1 $1 $ Function $2 }
      | qdotid Atom                            { withPos2 $1 $2 $ OpenIn (getName $1) $2 }
      | lazy Atom                              { withPos2 $1 $2 $ Lazy $2 }
      | Atom                                   { $1 }

Atom :: { Expr Parsed }
     : Var                                    { withPos1 $1 (VarRef (getL $1)) }
     | Con                                    { withPos1 $1 (VarRef (getL $1)) }
     | Lit                                    { withPos1 $1 (Literal (getL $1)) }
     | hole                                   { withPos1 $1 (Hole (Name (getHole $1))) }
     | '_'                                    { withPos1 $1 (Hole (Name (T.singleton '_'))) }
     | begin List1(Expr, ExprSep) end         { withPos2 $1 $3 $ Begin $2 }
     | '(' ')'                                { withPos2 $1 $2 $ Literal LiUnit }
     | '(' Section ')'                        { withPos2 $1 $3 $ Parens $2 }
     | '(' NullSection ',' List1(NullSection, ',') ')'
         { withPos2 $1 $5 $ tupleExpr ($2:$4) }
     | '{' List(ExprRow, ',') '}'             { withPos2 $1 $3 $ Record $2 }
     | '{' Expr with List1(ExprRow, ',') '}'  { withPos2 $1 $5 $ RecordExt $2 $4 }

     | Atom access                            { withPos2 $1 $2 $ Access $1 (getIdent $2) }

ExprBlock :: { Expr Parsed }
          : List1(Expr, ExprSep)              { completeTuple Begin $1 }

ExprSep :: { () }
        : ';'    { () }
        | '$sep' { () }

ExprIn :: { () }
        : in    { () }
        | '$in' { () }

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

ExprRow :: { Field Parsed }
  : ident '=' Expr         { withPos2 $1 $3 $ Field (getIdent $1) $3 }
  | ident                  { withPos1 $1    $ Field (getIdent $1) $ withPos1 $1 $ VarRef (getName $1) }

Var :: { Located (Var Parsed) }
    : ident  { lPos1 $1 $ getName $1 }
    | qident { lPos1 $1 $ getName $1 }

Con :: { Located (Var Parsed) }
    : conid      { lPos1 $1 $ getName $1 }
    | qconid     { lPos1 $1 $ getName $1 }

TyVar :: { Located (Var Parsed) }
      : tyvar { lPos1 $1 $ Name (getIdent $1) }

BindGroup :: { [Binding Parsed] }
          : Binding                           { [$1] }
          | BindGroup and Binding             { $3 : $1 }

Binding :: { Binding Parsed }
        : PreBinding { $1 }
        | implicit PreBinding { implicitify $1 $2 }

PreBinding :: { Binding Parsed }
           : Pattern PostBinding               { withPos1 $1 $ Matching $1 $2 }
           | Pattern ':' Type PostBinding      { withPos2 $1 $3 $ Matching $1 $ withPos2 $3 $4 $ Ascription $4 (getL $3) }

           | BindName ListE1(Parameter) PostBinding
             { Binding (getL $1) (foldr (\x y -> withPos2 x $3 (Fun x y)) $3 $2) BindRegular (withPos1 $1 id) }
           | BindName ListE1(Parameter) ':' Type PostBinding
             { Binding (getL $1) (foldr (\x y -> withPos2 x $5 (Fun x y)) (Ascription $5 (getL $4) (withPos2 $1 $5 id)) $2) BindRegular (withPos2 $1 $4 id) }

           | ArgP BindOp ArgP PostBinding
             { Binding (getL $2) (withPos2 $1 $4 (Fun (PatParam $1) (withPos2 $3 $4 (Fun (PatParam $3) $4)))) BindRegular (withPos2 $1 $3 id) }

PostBinding :: { Expr e }
  : '=' ExprBlock '$end'                       { $2 }

BindName :: { Located (Var Parsed) }
     : ident                                   { lPos1 $1 $ getName $1 }
     | '(' op ')'                              { lPos2 $1 $3 $ getName $2 }

BindOp :: { Located (Var Parsed) }
       : '*'                                   { lPos1 $1 $ Name (T.pack "*") }
       | '~'                                   { lPos1 $1 $ Name (T.pack "~") }
       | op                                    { lPos1 $1 $ getName $1 }
       | opid                                  { lPos1 $1 $ getName $1 }

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

Lit :: { Located Lit }
    : int                  { lPos1 $1 $ LiInt (getInt $1) }
    | float                { lPos1 $1 $ LiFloat (getFloat $1) }
    | string               { lPos1 $1 $ LiStr (getString $1) }
    | true                 { lPos1 $1 $ LiBool True }
    | false                { lPos1 $1 $ LiBool False }

-- An alternative to Pattern which uses TypeProd
-- instead of Type
MPattern :: { Pattern Parsed }
         : ArgP                   { $1 }
         | Con ArgP               { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
         | MPattern ':' TypeProd  { withPos2 $1 $3 $ PType $1 (getL $3) }

Pattern :: { Pattern Parsed }
        : ArgP                    { $1 }
        | Con ArgP                { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
        | Pattern ':' Type        { withPos2 $1 $3 $ PType $1 (getL $3) }

ArgP :: { Pattern Parsed }
     : ident                                      { withPos1 $1 $ Capture (getName $1) }
     | '_'                                        { withPos1 $1 $ Wildcard }
     | Con                                        { withPos1 $1 $ Destructure (getL $1) Nothing }
     | '{' List(PatternRow, ',') '}'              { withPos2 $1 $3 $ PRecord $2 }
     | '(' List(Pattern, ',') ')'                 { withPos2 $1 $3 $ tuplePattern $2 }
     | Lit                                        { withPos1 $1 (PLiteral (getL $1)) }

PatternRow :: { (T.Text, Pattern Parsed) }
  : ident '=' Pattern                             { (getIdent $1, $3) }
  | ident                                         { (getIdent $1, withPos1 $1 $ Capture(getName $1)) }

Arm :: { (Pattern Parsed, Expr Parsed) }
    : '|' List1(MPattern, ',') '->' ExprBlock  { (completeTuple PTuple $2, $4) }


Parameter :: { Parameter Parsed }
          : ArgP             { PatParam $1 }
          | iident           { ImplParam (withPos1 $1 $ Capture (getName $1)) }
          | '?(' Pattern ')' { ImplParam $2 }

Type :: { Located (Type Parsed) }
     : TypeProd                                   { $1 }
     | TypeProd '->' Type                         { lPos2 $1 $3 $ TyPi (Anon (getL $1)) (getL $3) }
     | TypeProd '=>' Type                         { lPos2 $1 $3 $ TyPi (Implicit (getL $1)) (getL $3) }
     | forall ListE1(tyvar) '.' Type              { lPos2 $1 $4 $ forallTy (map getName $2) (getL $4) }

TypeProd :: { Located (Type Parsed) }
         : TypeApp                                { $1 }
         | TypeApp '*' TypeProd                   { lPos2 $1 $3 $ TyTuple (getL $1) (getL $3) }

TypeApp  :: { Located (Type Parsed) }
         : TypeAtom                               { $1 }
         | TypeApp TypeAtom                       { lPos2 $1 $2 $ TyApp (getL $1) (getL $2) }

TypeAtom :: { Located (Type Parsed) }
         : Var                                    { lPos1 $1 $ TyCon (getL $1) }
         | TyVar                                  { lPos1 $1 $ TyVar (getL $1) }
         | Con                                    { lPos1 $1 $ TyPromotedCon (getL $1) }
         | type                                   { lPos1 $1 TyType }
         | lazy                                   { lPos1 $1 $ TyCon (Name (T.pack "lazy")) }
         | '(' ')'                                { lPos2 $1 $2 $ TyCon (Name (T.pack "unit")) }
         | '(' Type ')'                           { lPos2 $1 $3 (getL $2) }
         | '{' List(TypeRow, ',') '}'             { lPos2 $1 $3 $ TyExactRows $2 }
         | '{' Type '|' List(TypeRow, ',') '}'    { lPos2 $1 $5 $ TyRows (getL $2) $4 }

TypeRow :: { (T.Text, Type Parsed) }
  : ident ':' Type                                { (getIdent $1, getL $3) }

{

data Located a = L a Span

instance Spanned (Located a) where
  annotation (L _ s) = s

lexer :: (Token -> Parser a) -> Parser a
lexer = (lexerContextScan >>=)

parseError :: (Token, [String]) -> Parser a
parseError (tok, exp) = do
  stk <- pending <$> getState
  case findEof stk of
    Nothing -> failWith mainErr
    Just (Token _ _ e) -> failWiths [mainErr, UnexpectedEnd e]

  where
    mainErr = UnexpectedToken tok (nub (map unmap exp))

    findEof :: PendingState -> Maybe Token
    findEof (Working t@(Token TcEOF _ _)) = Just t
    findEof (Result _ s) = findEof s
    findEof _ = Nothing

    unmap :: String -> String
    unmap "'$end'" = friendlyName TcVEnd
    unmap "'$sep'" = friendlyName TcVSep
    unmap "'$in'" = friendlyName TcVIn
    unmap "'$begin'" = friendlyName TcVBegin
    unmap "op" = "operator"
    unmap "ident" = "identifier"
    unmap "opid" = "operator"
    unmap "conid" = "constructor"
    unmap "qident" = "identifier"
    unmap "qconid" = "constructor"
    unmap "qopid" = "operator"
    unmap "tyvar" = "type variable"
    unmap x = x

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

getIdent  (Token (TcOp x) _ _)         = x
getIdent  (Token (TcIdentifier x) _ _) = x
getIdent  (Token (TcOpIdent x) _ _)    = x
getIdent  (Token (TcConIdent x) _ _)   = x
getIdent  (Token (TcAccess x) _ _)     = x
getIdent  (Token (TcTyvar x) _ _)      = x

getName (Token (TcOp x) _ _)                = Name x
getName (Token (TcIdentifier x) _ _)        = Name x
getName (Token (TcQIdentifier x) _ _)       = Name x
getName (Token (TcOpIdent x) _ _)           = Name x
getName (Token (TcConIdent x) _ _)          = Name x
getName (Token (TcIdentifierQual ms x) _ _) = foldl (flip InModule) (Name x) ms
getName (Token (TcOpIdentQual ms x) _ _)    = foldl (flip InModule) (Name x) ms
getName (Token (TcConIdentQual ms x) _ _)   = foldl (flip InModule) (Name x) ms
getName (Token (TcDotQual ms) _ _)          = foldl (flip InModule) (Name (last ms)) (init ms)
getName (Token (TcTyvar x) _ _)             = Name x

getHole   (Token (TcHole x) _ _)       = x
getInt    (Token (TcInteger x) _ _)    = x
getFloat  (Token (TcFloat x) _ _)      = x
getString (Token (TcString  x) _ _)    = x
getL      (L x _)                    = x

forallTy vs t = foldr TyPi t (map (flip Invisible Nothing) vs)

implicitify :: Spanned a => a -> Binding Parsed -> Binding Parsed
implicitify x (Binding a b _ c) = Binding a b BindImplicit (withPos1 x id <> c)
implicitify x (Matching a b c) = ParsedBinding a b BindImplicit (withPos1 x id <> c)
implicitify x (ParsedBinding a b _ c) = ParsedBinding a b BindImplicit (withPos1 x id <> c)

respanFun :: (Spanned a, Spanned b) => a -> b -> Expr Parsed -> Expr Parsed
respanFun s e (Fun p b _) = Fun p b (mkSpanUnsafe (spanStart (annotation s)) (spanEnd (annotation e)))
respanFun _ _ _ = error "what"
}
