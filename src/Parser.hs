module Parser where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec

import Parser.Lexer

import Data.Functor.Identity

import Control.Monad
import Syntax

bindGroup :: Parser [(Var, Expr)]
bindGroup = sepBy1 decl (reserved "and") where
  decl = do
    x <- name
    ps <- many patternP
    reservedOp "="
    bd <- exprP
    case ps of
      [] -> pure (x, bd)
      _ -> pure (x, foldr Fun bd ps)

exprP' :: Parser Expr
exprP' = parens exprP
     <|> funExpr
     <|> letExpr
     <|> ifExpr
     <|> matchExpr
     <|> beginExpr
     <|> VarRef <$> name
     <|> Literal <$> lit where
  funExpr = do
    reserved "fun"
    x <- patternP
    reservedOp "->"
    Fun x <$> exprP
  letExpr = do
    reserved "let"
    bgs <- bindGroup
    reserved "in"
    Let bgs <$> exprP
  ifExpr = do
    reserved "if"
    c <- exprP
    reserved "then"
    t <- exprP
    reserved "else"
    If c t <$> exprP
  beginExpr = Begin <$> (reserved "begin" *> semiSep1 exprP <* reserved "end")
  matchExpr = do
    reserved "match"
    x <- exprP
    reserved "with"
    Match x <$> many1 (arm <?> "match arm")
  arm = do
    reservedOp "|"
    p <- many1 patternP
    case p of
      [x] -> do
        reservedOp "->"
        (,) x <$> exprP
      (Destructure v _:xs) -> do
        reservedOp "->"
        (,) (Destructure v xs) <$> exprP
      _ -> mzero


patternP :: Parser Pattern
patternP = wildcard <|> capture <|> constructor <|> destructure where
  wildcard = Wildcard <$ reservedOp "_"
  capture = Capture <$> varName
  varName = (Name <$> lowerIdent) <?> "variableName"
  constructor = flip Destructure [] <$> constrName
  destructure = parens $ do
    ps <- constrName
    Destructure ps <$> many1 patternP
  lowerIdent = lexeme $ do
    x <- lower
    (x:) <$> many (Tok.identLetter style)

exprP :: Parser Expr
exprP = exprOpP where
  expr' = foldl1 App <$> many1 exprP'
  exprOpP = buildExpressionParser table expr' <?> "expression"
  bop x = binary x (\a b -> BinOp a (VarRef (Name x)) b)
  table = [ [ bop "**" AssocRight ]
          , [ bop "*"  AssocLeft, bop "/" AssocLeft ]
          , [ bop "+"  AssocLeft, bop "-" AssocLeft ]
          , [ bop "^"  AssocLeft ]
          , [ bop "<"  AssocNone, bop ">" AssocNone
            , bop ">=" AssocNone, bop "<=" AssocNone
            , bop "==" AssocNone, bop "<>" AssocNone ]
          , [ bop "&&" AssocNone ]
          , [ bop "||" AssocNone ] ]

typeP :: Parser Type
typeP = typeOpP where
  typeOpP = buildExpressionParser table type' <?> "type"
  type' = foldl1 TyApp <$> many1 typeP'
  table = [ [ binary "->" TyArr AssocRight ]]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary n f a = flip Infix a $ do
  reservedOp n
  pure f

typeP' :: Parser Type
typeP' = parens typeP
     <|> TyVar <$> tyVar
     <|> tyCon
     <|> tyForall where
  tyForall = do
    reserved "forall"
    nms <- commaSep1 tyVar
    _ <- dot
    cs <- parens . commaSep1 $ typeP
    reservedOp "=>"
    TyForall nms cs <$> typeP
  tyCon = TyCon <$> name

tyVar :: Parser String
tyVar = lexeme $ do
  _ <- char '\''
  x <- Tok.identStart style
  (x:) <$> many (Tok.identLetter style)

name :: Parser Var
name = Name <$> identifier

constrName :: Parser Var
constrName = (Name <$> upperIdent) <?> "constructor name" where
  upperIdent = lexeme $ do
    x <- upper
    (x:) <$> many (Tok.identLetter style)

lit :: Parser Lit
lit = intLit <|> strLit <|> true <|> false where
  intLit = LiInt <$> natural
  strLit = LiStr <$> stringLiteral
  true = LiBool True <$ reserved "true"
  false = LiBool False <$ reserved "false"

toplevelP :: Parser Toplevel
toplevelP = letStmt <|> try foreignVal <|> valStmt <|> dataDecl where
  letStmt = do
    reserved "let"
    LetStmt <$> bindGroup
  valStmt = do
    reserved "val"
    x <- name
    _ <- colon
    ValStmt x <$> typeP
  foreignVal = do
    reserved "val"
    reserved "foreign"
    x <- name
    n <- stringLiteral
    _ <- colon
    ForeignVal x n <$> typeP
  dataDecl = do
    reserved "type"
    x <- name
    xs <- many tyVar
    reservedOp "="
    cs <- many $ do
      reservedOp "|"
      x <- constrName
      (,) x <$> sepBy typeP (reservedOp "*")
    pure $ TypeDecl x xs cs

program :: Parser [Toplevel]
program = semiSep1 toplevelP <* eof
