{
{-| The parser for the Amulet compiler, as defined by a Happy grammar.

  == Shift/reduce conflicts within the grammar
  The grammar has several shift/reduce conflicts which are documented as
  follows.

  === In record expressions (+1)
  When the input contains @{ foo : int ... }@, the parser does not know whether
  @foo@ refers to a table key. Without further lookahead, the input could be a
  record key (@{ foo : int = 2 }@) or an expression within an extension (@{ foo
  : int with ...}@).

  Currently the latter of these is chosen, as Happy always prefers to shift.

  === In type annotations and ascriptions (+6)

  There's several cases where type ascriptions cause ambiguities. As
  these fall into one state, we'll group them here:

  Operators after type ascriptions on expressions could refer to the
  expression or type. For instanece, @a : int * int@ could be parsed as
  @(a : int) * int@ or @a : (int * int)@. Currently the latter is chosen.

  There is also potential for a conflict for expressions within pattern
  guards. For @| _ when a -> b@, the arrow could either be part of an
  arrow type in the ascription, or the separator of the pattern arm. In
  this case, the former is chosen.

-}
{-# LANGUAGE ViewPatterns #-}
module Parser
  ( parseTops
  , parseRepl
  , parseReplExpr
  , parseInfoVar
  , parseType

  , Located, getL
  ) where

import Control.Arrow (second)

import Data.List (intercalate, nub)
import Data.Maybe (fromJust, isJust)
import Data.Span
import Data.Spanned
import Data.Semigroup
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Text as T

import Parser.Precedence
import Parser.Wrapper
import Parser.Context
import Parser.Error
import Parser.Lexer
import Parser.Token

import Syntax.Var
import Syntax

#undef __GLASGOW_HASKELL__
#define __GLASGOW_HASKELL__ 709

}

%name parseTops Tops
%name parseRepl Repl
%name parseReplExpr ReplExpr
%name parseInfoVar Reference
%name parseType Type

%tokentype { Token }
%monad { Parser } { (>>=) } { return }
%lexer { lexer } { Token TcEOF _ _ }
%error { parseError }
%errorhandlertype explist

%token
  '->'     { Token TcArrow _ _ }
  '<-'     { Token TcGenerator _ _ }
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
  as       { Token TcAs _ _ }
  class    { Token TcClass _ _ }
  instance { Token TcInstance _ _ }
  when     { Token TcWhen _ _ }
  private  { Token TcPrivate _ _ }

  ','      { Token TcComma _ _ }
  '.'      { Token TcDot _ _ }
  ':'      { Token TcColon _ _ }
  ';;'     { Token TcTopSep _ _ }
  ';'      { Token TcSemicolon _ _ }
  '('      { Token TcOParen _ _ }
  ')'      { Token TcCParen _ _ }
  '@'      { Token TcAt _ _ }
  '{'      { Token TcOBrace _ _ }
  '}'      { Token TcCBrace _ _ }
  '['      { Token TcOSquare _ _ }
  ']'      { Token TcCSquare _ _ }
  '!'      { Token TcBang _ _ }

  op       { Token (TcOp _) _ _ }
  ident    { Token (TcIdentifier _) _ _ }
  opid     { Token (TcOpIdent _) _ _ }
  conid    { Token (TcConIdent _) _ _ }
  qident   { Token (TcIdentifierQual _ _) _ _ }
  qopid    { Token (TcOpIdentQual _ _) _ _ }
  qconid   { Token (TcConIdentQual _ _) _ _ }
  qdotid   { Token (TcDotQual _) _ _ }
  access   { Token (TcAccess _) _ _ }
  dotop    { Token (TcDotOp _) _ _ }
  tyvar    { Token (TcTyvar _) _ _ }
  hole     { Token (TcHole _) _ _ }
  int      { Token (TcInteger _) _ _ }
  float    { Token (TcFloat _) _ _ }
  string   { Token (TcString  _) _ _ }

  '$begin' { Token TcVBegin _ _ }
  '$end'   { Token TcVEnd _ _ }
  '$in'    { Token TcVIn _ _ }
  '$sep'   { Token TcVSep _ _ }

-- Please try to update the module documentation when this number changes.
%expect 7

%%

Tops :: { [Toplevel Parsed] }
     : List1(Top, TopSep)                      { $1 }

-- | An access modifier for top-level definitions
--
-- We inline this within let binding groups and modules to remove a
-- shift/reduce conflict.  We do not know if let should be
-- followed by an Access or not). It's slightly absurd that inlining the
-- production works, but at the same time makes perfect sense.
Access :: { TopAccess }
  :                                            { Public }
  | private                                    { Private }

TopSep :: { () }
    : ';;'   { () }
    | '$sep' { () }

Top :: { Toplevel Parsed }
    -- See comment on 'Access' as to why this is inlined
    : let BindGroup                            { LetStmt Public (reverse $2) }
    | let private BindGroup                    { LetStmt Private (reverse $3) }

    | external Access val BindName ':' Type '=' string
      { withPos2 $1 $8 $ ForeignVal $2 (getL $4) (getString $8) (getL $6) }

    | Access type BindName ListE(TyConArg) TypeBody { withPos2 $2 $3 $ TypeDecl $1 (getL $3) $4 $5 }
    | Access type TyConArg BindOp TyConArg TypeBody { withPos2 $2 $4 $ TypeDecl $1 (getL $4) [$3, $5] $6 }

    | Access type BindName ListE(TyConArg) '<-' Type
        { withPos2 $2 $3 $ TySymDecl $1 (getL $3) $4 (getL $6) }
    | Access type TyConArg BindOp TyConArg '<-' Type
        { withPos2 $2 $4 $ TySymDecl $1 (getL $4) [$3, $5] (getL $7) }

    | Access type function BindName ListE1(TyConArg) TyFunKindSig Begin(TyFunBody)
      -- 1    2     3       4       5               6             7
        { withPos2 $2 $7 $ TypeFunDecl $1 (getL $4) $5 $6 (getL $7) }

    | module qconid '=' Begin(Tops)            { Module Public (getName $2) (getL $4) }
    | private module qconid '=' Begin(Tops)    { Module Private (getName $3) (getL $5) }
    | module conid '=' Begin(Tops)             { Module Public (getName $2) (getL $4) }
    | private module conid '=' Begin(Tops)     { Module Private (getName $3) (getL $5) }
    | module conid '=' Con                     { Open (getL $4) (Just (getIdent $2)) }

    -- Note, we use fmap rather than <$>, as Happy's parser really doesn't like that.
    | class Type Fundeps Begin(ClassItems) 
        {% fmap (withPos2 $1 $4) $ buildClass Public $2 $3 (getL $4) }
    | private class Type Fundeps Begin(ClassItems)
        {% fmap (withPos2 $1 $5) $ buildClass Private $3 $4 (getL $5) }

    | instance Type Begin(Methods)             {% fmap (withPos2 $1 $3) $ buildInstance $2 (getL $3) }

    | open Con                                 { Open (getL $2) Nothing }

TyFunBody :: { [TyFunClause Parsed] }
  : List(TyFunEq, TopSep) { $1 }

TyFunEq :: { TyFunClause Parsed }
  : Type '=' Type { withPos2 $1 $3 $ TyFunClause (getL $1) (getL $3) }

TyFunKindSig :: { Maybe (Type Parsed) }
  : ':' Type { Just (getL $2) }
  |          { Nothing }

Begin(a)
  : begin a end                             { lPos2 $1 $3 $2 }
  | '$begin' a '$end'                       { lPos2 $1 $3 $2 }

TypeBody :: { Maybe [Constructor Parsed] }
  :                                            { Nothing }
  | '=' List1(Ctor, '|')                       { Just $2 }
  | '=' '|' List1(Ctor, '|')                   { Just $3 }
  | '=' '|'                                    { Just [] }

ClassItems :: { [ClassItem Parsed] }
  : List(ClassItem, TopSep) { $1 }

ClassItem :: { ClassItem Parsed }
  : val BindName ':' Type { withPos2 $1 $4 $ MethodSig (getL $2) (getL $4) }
  | let Binding { withPos2 $1 $2 $ DefaultMethod $2 }
  | type BindName ':' Type { withPos2 $1 $4 $ AssocType (getL $2) (getL $4) }
  | type BindName          { withPos2 $1 $2 $ AssocType (getL $2) TyType }

Fundeps :: { [Fundep Parsed] }
  : '|' List1(Fundep, ',') { $2 }
  |                        { [] }

Fundep :: { Fundep Parsed }
  : ListE1(TyVar) '->' ListE1(TyVar) { withPos2 (head $1) (last $3) $ Fundep (map getL $1) (map getL $3) }

Methods :: { [InstanceItem Parsed] }
  : List(Method, TopSep) { $1 }

Method :: { InstanceItem Parsed }
  : let Binding { MethodImpl $2 }
  | type BindName ListE(TyConArg) '=' Type
    { withPos2 $1 $5 $ TypeImpl (getL $2) $3 (getL $5) }

TyConArg :: { TyConArg Parsed }
         : TyVar { TyVarArg (getL $1) }
         | '(' TyVar ':' Type ')' { TyAnnArg (getL $2) (getL $4) }

Ctor :: { Constructor Parsed }
     : Access conid                                   { withPos1 $2    $ UnitCon $1 (getName $2) }
     | Access conid of Type                           { withPos2 $2 $4 $ ArgCon  $1 (getName $2) (getL $4) }
     | Access conid ':' Type                          { withPos2 $2 $4 $ GadtCon $1 (getName $2) (getL $4) }

Repl :: { Either (Toplevel Parsed) (Expr Parsed) }
     : Top   ReplSep                           { Left $1 }
     | Expr  ReplSep                           { Right $1 }

ReplExpr :: { Expr Parsed }
  : Expr ReplSep { $1 }

ReplSep :: { () }
    : ';;'   { () }
    | '$sep' { () }
    |        { () }

Exprs :: { Expr Parsed }
  : List1(Expr, ',')                           { completeTuple Tuple $1 }

Expr :: { Expr Parsed }
  : ExprOp                                     { fixupExpr $1 }

ExprOp :: { Expr Parsed }
  : ExprDotOp                                  { $1 }
  | ExprOp Infix ExprDotOp                     { withPos2 $1 $3 $ BinOp $1 $2 $3 }

ExprDotOp :: { Expr Parsed  }
  : ExprTyApp                                  { $1 }
  | ExprDotOp '.' '(' Exprs ')'                { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ T.pack ".()") $4 }
  | ExprDotOp '.' '{' Exprs '}'                { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ T.pack ".{}") $4 }
  | ExprDotOp '.' '[' Exprs ']'                { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ T.pack ".[]") $4 }
  | ExprDotOp dotop '(' Exprs ')'              { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ getIdent $2 <> T.pack "()") $4 }
  | ExprDotOp dotop '{' Exprs '}'              { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ getIdent $2 <> T.pack "{}") $4 }
  | ExprDotOp dotop '[' Exprs ']'              { withPos2 $1 $5 $ BinOp $1 (withPos2 $2 $3 . VarRef . Name $ getIdent $2 <> T.pack "[]") $4 }

ExprTyApp :: { Expr Parsed }
  : ExprApp                                    { $1 }
  | ExprTyApp ':' Type                         { withPos2 $1 $3 $ Ascription $1 (getL $3) }

ExprApp :: { Expr Parsed }
        : Expr0                    { $1 }
        | ExprApp PreAtom          { withPos2 $1 $2 $ App $1 $2 }
        | ExprApp '@' TypeAtom     { withPos2 $1 $3 $ Vta $1 (getL $3) }

Expr0 :: { Expr Parsed }
      : fun ListE1(Parameter) '->' ExprBlock '$end'
        { respanFun $1 $4 $ foldr (\x y -> withPos2 x $4 $ Fun x y) $4 $2 }
      | let BindGroup ExprIn ExprBlock '$end'  { withPos2 $1 $4 $ Let (reverse $2) $4 }
      | let open Con ExprIn ExprBlock '$end'   { withPos2 $1 $5 $ OpenIn (getL $3) $5 }
      | if Expr then ExprBlock else ExprBlock '$end'
          { withPos2 $1 $6 $ If $2 $4 $6 }
      | match Exprs with ListE1(Arm) '$end'    { withPos2 $1 $3 $ Match $2 $4 }
      | match Exprs with '(' ')'               { withPos2 $1 $3 $ Match $2 [] }
      | function ListE1(Arm) '$end'            { withPos1 $1 $ Function $2 }
      | function '(' ')'                       { withPos1 $1 $ Function [] }
      | lazy PreAtom                           { withPos2 $1 $2 $ Lazy $2 }
      | PreAtom                                { $1 }

-- | A 'prefixed' atom.
--
-- This is required in order to avoid shift-reduce conflicts: otherwise
-- 'M. foo.bar' could be M.(foo.bar) or M.(foo).bar
PreAtom :: { Expr Parsed }
     : Atom                                   { $1 }
     | qdotid Atom                            { withPos2 $1 $2 $ OpenIn (getName $1) $2 }
     | '!' Atom                               { makeBang $1 $2 }

Atom :: { Expr Parsed }
     : Var                                    { withPos1 $1 (VarRef (getL $1)) }
     | Con                                    { withPos1 $1 (VarRef (getL $1)) }
     | Lit                                    { withPos1 $1 (Literal (getL $1)) }
     | hole                                   { withPos1 $1 (Hole (Name (getHole $1))) }
     | '_'                                    { withPos1 $1 (Hole (Name (T.singleton '_'))) }
     | begin List1(CompStmt, ExprSep) end     { withPos2 $1 $3 $ DoExpr bindVar $2 }
     | '(' ')'                                { withPos2 $1 $2 $ Literal LiUnit }
     | '(' Section ')'                        { withPos2 $1 $3 $ Parens $2 }
     | '(' NullSection ',' List1(NullSection, ',') ')'
         { withPos2 $1 $5 $ tupleExpr ($2:$4) }
     | '{' ListT(ExprRow, ',') '}'            { withPos2 $1 $3 $ Record $2 }
     | '{' Expr with List1T(ExprRow, ',') '}' { withPos2 $1 $5 $ RecordExt $2 $4 }
     | '[' List(Expr, ',') ']'                { withPos2 $1 $3 $ ListExp $2 }
     | '[' Expr '|' List1(CompStmt, ',') ']'  { withPos2 $1 $5 $ ListComp $2 $4 }
     | Atom access                            { withPos2 $1 $2 $ Access $1 (getIdent $2) }

CompStmt :: { CompStmt Parsed }
  : let BindGroup                             { withPos1 $1 $ CompLet (reverse $2) }
  | BasicStmt                                 { $1 }
  | Expr                                      { CompGuard $1 }

BasicStmt :: { CompStmt Parsed }
  : with Pattern '<-' Expr                    { withPos2 $1 $4 $ CompGen $2 $4 }

-- | Computations which are not valid in an expression context. Allows us to
-- produce somewhat more friendly error messages in the common case.
ExprNonComp :: { Expr Parsed }
  : Expr                                      { $1 }
  | BasicStmt                                 {% tellErrors [MisplacedWith (annotation $1)]
                                                 *> pure (withPos1 $1 (Literal LiUnit)) }

ExprBlock :: { Expr Parsed }
  : List1(ExprNonComp, ExprSep)               { completeTuple Begin $1 }

ExprSep :: { () }
        : ';'    { () }
        | '$sep' { () }

ExprIn :: { () }
        : in    { () }
        | '$in' { () }

InfixName :: { Located (Var Parsed) }
  : '*'                                       { lPos1 $1 . Name $ T.pack "*" }
  | '~'                                       { lPos1 $1 . Name $ T.pack "~" }
  | op                                        { lPos1 $1 $ getName $1 }
  | opid                                      { lPos1 $1 $ getName $1 }
  | qopid                                     { lPos1 $1 $ getName $1 }

Infix :: { Expr Parsed }
  : InfixName                                 { withPos1 $1 $ VarRef (getL $1) }

OperatorName :: { Located (Var Parsed) }
  : InfixName                                 { $1 }
  | '!'                                       { lPos1 $1 . Name $ T.pack "!" }

  | '.' '(' ')'                               { lPos2 $1 $3 . Name $ T.pack ".()" }
  | '.' '{' '}'                               { lPos2 $1 $3 . Name $ T.pack ".{}" }
  | '.' '[' ']'                               { lPos2 $1 $3 . Name $ T.pack ".[]" }
  | dotop '(' ')'                             { lPos2 $1 $3 . Name $ getIdent $1 <> T.pack "()" }
  | dotop '{' '}'                             { lPos2 $1 $3 . Name $ getIdent $1 <> T.pack "{}" }
  | dotop '[' ']'                             { lPos2 $1 $3 . Name $ getIdent $1 <> T.pack "[]" }

Operator :: { Expr Parsed }
  : OperatorName                              { withPos1 $1 $ VarRef (getL $1) }

Section :: { Expr Parsed }
        : Expr                                { $1 }
        | access                              { withPos1 $1 $ AccessSection (getIdent $1) }
        | Operator                            { withPos1 $1 $ BothSection $1 }
        | ExprOp Infix                        { withPos2 $1 $2 $ RightSection (fixupExpr $1) $2 }
        | Infix ExprOp                        { withPos2 $1 $2 $ LeftSection $1 (fixupExpr $2) }

Reference :: { Located (Var Parsed) }
  : Var                                       { $1 }
  | Con                                       { $1 }
  | '(' OperatorName    ')'                   { $2 }

NullSection :: { Maybe (Expr Parsed) }
  :                                           { Nothing }
  | Section                                   { Just $1 }

ExprRow :: { Field Parsed }
  : ident OptType '=' Expr                    { withPos2 $1 $4 $ Field (getIdent $1) $ $2 $4 }
  | ident OptType
    { withPos1 $1    $ Field (getIdent $1) $ $2 $ withPos1 $1 $ VarRef (getName $1) }

OptType :: { Expr Parsed -> Expr Parsed }
  :                                           { id }
  | ':' Type                                  { withPos1 $2 . flip Ascription (getL $2) }


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
        : BPattern PostBinding              { withPos1 $1 $ Matching $1 $2 }
        | BPattern ':' Type PostBinding
          { withPos2 $1 $3 $ Matching $1 $ withPos2 $3 $4 $ Ascription $4 (getL $3) }

        | BindName ListE1(Parameter) PostBinding
          { Binding (getL $1) (foldr (\x y -> withPos2 x $3 (Fun x y)) $3 $2) True (withPos1 $1 id) }
        | BindName ListE1(Parameter) ':' Type PostBinding
          { Binding (getL $1)
             (foldr (\x y -> withPos2 x $5 (Fun x y)) (Ascription $5 (getL $4) (withPos2 $1 $5 id)) $2)
             True
             (withPos2 $1 $4 id) }

        | ArgP BindOp ArgP PostBinding
          { Binding (getL $2)
              (withPos2 $1 $4 (Fun (PatParam $1) (withPos2 $3 $4 (Fun (PatParam $3) $4))))
              True
              (withPos2 $1 $3 id) }

PostBinding :: { Expr Parsed }
  : '=' ExprBlock '$end'                       { $2 }

BindName :: { Located (Var Parsed) }
  : ident                                      { lPos1 $1 $ getName $1 }
  | '(' OperatorName ')'                       {% checkUnqualified $2 }

BindOp :: { Located (Var Parsed) }
  : InfixName                                  {% checkUnqualified $1 }

-- | A list with a separator
List(p, s)
    : {- Empty -}       { [] }
    | List1(p, s)       { $1 }

-- | A non-empty list with a separator
List1(p, s)
     : p                { [$1] }
     | p s List1(p, s)  { $1 : $3 }

-- | A list with an optional trailing separator
ListT(p, s)
    -- We don't want to parse empty lists with a trailing comm
  : {- Empty -}         { [] }
  | List1T(p, s)        { $1 }

-- | A non-empty list with an optional trailing separator
List1T(p, s)
  : p                   { [$1] }
  | p s                 { [$1] }
  | p s List1T(p, s)    { $1 : $3 }

-- | A list with no separator
ListE(p)
     : {- Empty -}      { [] }
     | ListE1(p)        { $1 }

-- | A non-empty list with no separator
ListE1(p)
     : p                { [$1] }
     | p ListE1(p)      { $1 : $2 }

Lit :: { Located Lit }
    : int                  { lPos1 $1 $ LiInt (getInt $1) }
    | float                { lPos1 $1 $ LiFloat (getFloat $1) }
    | string               { lPos1 $1 $ LiStr (getString $1) }
    | true                 { lPos1 $1 $ LiBool True }
    | false                { lPos1 $1 $ LiBool False }

-- | An alternative to Pattern which uses TypeOp instead of Type,
-- suitable for usage in match arms.
--
-- This ensures '->' does not occur on the top level of a type annotation, and
-- so there is no conflict with the '->' in an arm.
MPattern :: { Pattern Parsed }
         : ArgP                   { $1 }
         | Con ArgP               { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
         | MPattern ':' TypeOp    { withPos2 $1 $3 $ PType $1 (getL $3) }
         | MPattern as Var        { withPos2 $1 $3 $ PAs $1 (getL $3) }

-- | An alternative to Pattern without any type pattern, suitable for usage in
-- bindings.
--
-- We place the type annotation on the expression instead of within the pattern,
-- as that allows for easier desugaring later on.
BPattern :: { Pattern Parsed }
         : ArgP                   { $1 }
         | Con ArgP               { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
         | BPattern as Var        { withPos2 $1 $3 $ PAs $1 (getL $3) }

Pattern :: { Pattern Parsed }
        : ArgP                    { $1 }
        | Con ArgP                { withPos2 $1 $2 $ Destructure (getL $1) (Just $2) }
        | Pattern ':' Type        { withPos2 $1 $3 $ PType $1 (getL $3) }
        | Pattern as Var          { withPos2 $1 $3 $ PAs $1 (getL $3) }

ArgP :: { Pattern Parsed }
     : BindName                                   { withPos1 $1 $ Capture (getL $1) }
     | '_'                                        { withPos1 $1 $ Wildcard }
     | Con                                        { withPos1 $1 $ Destructure (getL $1) Nothing }
     | '{' ListT(PatternRow, ',') '}'             { withPos2 $1 $3 $ PRecord $2 }
     | '(' List(Pattern, ',') ')'                 { withPos2 $1 $3 $ tuplePattern $2 }
     | Lit                                        { withPos1 $1 (PLiteral (getL $1)) }
     | '[' List(Pattern, ',') ']'                 { withPos2 $1 $3 $ PList $2 }

PatternRow :: { (T.Text, Pattern Parsed) }
  : ident '=' Pattern                             { (getIdent $1, $3) }
  | ident                                         { (getIdent $1, withPos1 $1 $ Capture(getName $1)) }

Arm :: { Arm Parsed }
    : '|' List1(MPattern, ',') Guard '->' ExprBlock { Arm (completeTuple PTuple $2) $3 $5 }

Guard :: { Maybe (Expr Parsed) }
  :                                               { Nothing }
  | when Expr                                     { Just $2 }

Parameter :: { Parameter Parsed }
          : ArgP             { PatParam $1 }

Type :: { Located (Type Parsed) }
  : TypeOp                                        { $1 }
  | TypeOp '->' Type                              { lPos2 $1 $3 $ TyPi (Anon (getL $1)) (getL $3) }
  | TypeOp '=>' Type                              { lPos2 $1 $3 $ TyPi (Implicit (getL $1)) (getL $3) }
  | forall ListE1(ForallBinder) '.' Type          { lPos2 $1 $4 $ forallTy Spec $2 (getL $4) }
  | forall ListE1(ForallBinder) '->' Type         { lPos2 $1 $4 $ forallTy Req $2 (getL $4) }

ForallBinder :: { (Var Parsed, Maybe (Type Parsed)) }
  : tyvar       { (getName $1, Nothing) }
  | '(' tyvar ':' Type ')'
    { (getName $2, Just (getL $4)) }

TypeOp :: { Located (Type Parsed) }
  : TypeOp_                                       { fmap fixupType $1 }

TypeOp_ :: { Located (Type Parsed) }
  : TypeApp                                       { $1 }
  | TypeOp_ TypeOperator TypeApp                  { lPos2 $1 $3 $ TyOperator (getL $1) $2 (getL $3) }

TypeApp :: { Located (Type Parsed) }
  : TypeAtom                                      { $1 }
  | TypeApp TypeAtom                              { lPos2 $1 $2 $ TyApp (getL $1) (getL $2) }

TypeOperator :: { Var Parsed  }
  : InfixName                                     { getL $1 }

TypeOperatorF :: { Var Parsed }
  : TypeOperator                                  { $1 }
  | '->'                                          { Name $ T.pack "->" }

TypeAtom :: { Located (Type Parsed) }
         : Var                                    { lPos1 $1 $ TyCon (getL $1) }
         | TyVar                                  { lPos1 $1 $ TyVar (getL $1) }
         | Con                                    { lPos1 $1 $ TyPromotedCon (getL $1) }
         | type                                   { lPos1 $1 TyType }
         | lazy                                   { lPos1 $1 $ TyCon (Name (T.pack "lazy")) }
         | '(' ')'                                { lPos2 $1 $2 $ TyCon (Name (T.pack "unit")) }
         | '(' Type ')'                           { lPos2 $1 $3 $ TyParens (getL $2) }
         | '(' TypeOperatorF ')'                  { lPos2 $1 $3 $ TyParens (TyCon $2) }
         | '{' ListT(TypeRow, ',') '}'            { lPos2 $1 $3 $ TyExactRows $2 }
         | '{' Type '|' ListT(TypeRow, ',') '}'   { lPos2 $1 $5 $ TyRows (getL $2) $4 }
         | '_'                                    { lPos1 $1 (TyWildcard Nothing) }
         | string                                 { lPos1 $1 $ TyLit (LiStr (getString $1)) }
         | int                                    { lPos1 $1 $ TyLit (LiInt (getInt $1)) }

TypeRow :: { (T.Text, Type Parsed) }
  : ident ':' Type                                { (getIdent $1, getL $3) }

{

data Located a = L a Span
  deriving (Eq, Show, Ord)

instance Spanned (Located a) where
  annotation (L _ s) = s

instance Functor Located where
  fmap f (L a s) = L (f a) s

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

bindVar = Name (T.pack ">>=")

getIdent  (Token (TcOp x) _ _)         = x
getIdent  (Token (TcIdentifier x) _ _) = x
getIdent  (Token (TcOpIdent x) _ _)    = x
getIdent  (Token (TcConIdent x) _ _)   = x
getIdent  (Token (TcAccess x) _ _)     = x
getIdent  (Token (TcDotOp x) _ _)      = x
getIdent  (Token (TcTyvar x) _ _)      = x

getName (Token (TcIdentifierQual ms x) _ _) = foldl (flip InModule) (Name x) ms
getName (Token (TcOpIdentQual ms x) _ _)    = foldl (flip InModule) (Name x) ms
getName (Token (TcConIdentQual ms x) _ _)   = foldl (flip InModule) (Name x) ms
getName (Token (TcDotQual ms) _ _)          = foldl (flip InModule) (Name (last ms)) (init ms)
getName x                                   = Name (getIdent x)

getHole   (Token (TcHole x) _ _)       = x
getInt    (Token (TcInteger x) _ _)    = x
getFloat  (Token (TcFloat x) _ _)      = x
getString (Token (TcString  x) _ _)    = x
getL      (L x _)                    = x

forallTy spec vs t = foldr TyPi t (map (\(x, k) -> Invisible x k spec) vs)

respanFun :: (Spanned a, Spanned b) => a -> b -> Expr Parsed -> Expr Parsed
respanFun s e (Fun p b _) = Fun p b (mkSpanUnsafe (spanStart (annotation s)) (spanEnd (annotation e)))
respanFun _ _ _ = error "what"

buildClass :: TopAccess -> Located (Type Parsed) -> [Fundep Parsed]
           -> [ClassItem Parsed] -> Parser (Span -> Toplevel Parsed)
buildClass am (L parsed typ) fundep ms =
  case parsed of
    (TyPi (Implicit ctx) ty) -> do
      (name, ts) <- go [] ty
      pure (Class name am (Just ctx) ts fundep ms)
    ty -> do
      (name, ts) <- go [] ty
      pure (Class name am Nothing ts fundep ms)
  where
    go :: [TyConArg Parsed] -> Type Parsed -> Parser (Var Parsed, [TyConArg Parsed])
    go ts (TyCon v) = pure (v, ts)
    go ts (TyParens t) = go ts t
    go ts (TyApp rest (TyVar v)) = go (TyVarArg v:ts) rest
    go ts ty = do
      tellErrors [MalformedClass typ parsed]
      pure (undefined, ts)

makeBang :: _ -> Expr Parsed -> Expr Parsed
makeBang bang expr = withPos2 bang expr $ App derefref expr where
  derefref = withPos1 bang (VarRef (Name (T.pack "!")))

buildInstance :: Located (Type Parsed) -> [InstanceItem Parsed]
           -> Parser (Span -> Toplevel Parsed)
buildInstance (L ty typ) ms =
  case ty of
    (TyPi (Implicit ctx) ty) -> do
      name <- go ty
      pure (Instance name (Just ctx) ty ms)
    ty -> do
      name <- go ty
      pure (Instance name Nothing ty ms)
  where
    go :: Type Parsed -> Parser (Var Parsed)
    go (TyCon v) = pure v
    go (TyApp rest _) = go rest
    go ty = do
      tellErrors [MalformedInstance typ ty]
      pure (Name (error "malformed instance"))

checkUnqualified :: Located (Var Parsed) -> Parser (Located (Var Parsed))
checkUnqualified v@(L Name{} _) = pure v
checkUnqualified v@(L name loc) = tellErrors [BindQualified name loc] >> pure v

}
