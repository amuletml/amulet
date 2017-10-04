{-# LANGUAGE TupleSections, TypeFamilies #-}

module Parser where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Expr
import Text.Parsec
import Data.Span

import qualified Data.Text as T

import Parser.Lexer

import Data.Functor.Identity

import Control.Monad
import Syntax

type Expr' = Expr 'ParsePhase
type Toplevel' = Toplevel 'ParsePhase
type Type' = Type 'ParsePhase
type Pattern' = Pattern 'ParsePhase

data BeginStmt
  = BeginLet [(Var 'ParsePhase, Expr')]
  | BeginRun Expr'
  deriving (Eq)

bindGroup :: Parser [(Var 'ParsePhase, Expr')]
bindGroup = sepBy1 decl (reserved "and") where
  decl = do
    x <- name
    ps <- many patternP
    reservedOp "="
    bd <- exprP
    case ps of
      [] -> pure (x, bd)
      _ -> do
        let fun' pt b = Fun pt b (extract bd)
        pure (x, foldr fun' bd ps)

withPos :: Parser (Span -> a) -> Parser a
withPos k = do
  begin <- getPosition
  k' <- k
  end <- getPosition
  case mkSpan begin end of
    Nothing -> mzero <?> (sourceName begin ++ " to be the same file as " ++ sourceName end)
    Just vl -> pure (k' vl)

exprP' :: Parser Expr'
exprP' = parens exprP
     <|> funExpr
     <|> letExpr
     <|> ifExpr
     <|> matchExpr
     <|> beginExpr
     <|> withPos (VarRef <$> name)
     <|> withPos (Literal <$> lit) where
  funExpr = withPos $ do
    reserved "fun"
    x <- patternP
    reservedOp "->"
    Fun x <$> exprP
  letExpr = withPos $ do
    reserved "let"
    bgs <- bindGroup
    reserved "in"
    Let bgs <$> exprP
  ifExpr = withPos $ do
    reserved "if"
    c <- exprP
    reserved "then"
    t <- exprP
    reserved "else"
    If c t <$> exprP
  beginExpr = withPos $ do
    reserved "begin"
    x <- semiSep1 exprP
    reserved "end"
    pure $ Begin x
  matchExpr = withPos $ do
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

patternP :: Parser Pattern'
patternP = wildcard <|> capture <|> constructor <|> try pType <|> destructure where
  constructor, destructure, pType, capture :: Parser Pattern'
  wildcard :: Parser (Pattern p)
  wildcard = Wildcard <$ reservedOp "_"
  capture = Capture <$> varName
  varName = (Name <$> lowerIdent) <?> "variableName"
  constructor = flip Destructure [] <$> constrName
  destructure = parens $ do
    ps <- constrName
    Destructure ps <$> many1 patternP
  lowerIdent = lexeme $ do
    x <- lower
    T.pack . (x:) <$> many (Tok.identLetter style)
  pType = parens $ do
    x <- patternP
    reservedOp ":"
    PType x <$> typeP

exprP :: Parser Expr'
exprP = exprOpP where
  expr' = do
    (hd, tl, pos) <- withPos $ do
      x <- exprP'
      y' <- many exprP'
      pure (x, y',)
    let app' a b = App a b pos
    pure $ foldl app' hd tl
  exprOpP = buildExpressionParser table expr' <?> "expression"
  bop x = binary x (\p a b -> BinOp a (VarRef (Name (T.pack x)) p) b p :: Expr 'ParsePhase)
  table = [ [ bop "**" AssocRight ]
          , [ bop "*"  AssocLeft, bop "/" AssocLeft ]
          , [ bop "+"  AssocLeft, bop "-" AssocLeft ]
          , [ bop "^"  AssocLeft ]
          , [ bop "<"  AssocNone, bop ">" AssocNone
            , bop ">=" AssocNone, bop "<=" AssocNone
            , bop "==" AssocNone, bop "<>" AssocNone ]
          , [ bop "&&" AssocNone ]
          , [ bop "||" AssocNone ] ]

typeP :: Parser Type'
typeP = typeOpP where
  typeOpP = buildExpressionParser table type' <?> "type"
  type' = foldl1 TyApp <$> many1 typeP'
  table :: [[ Operator T.Text () Identity (Type p) ]]
  table = [ [ binary "->" (const TyArr) AssocRight ]]

binary :: String -> (Span -> a -> a -> a) -> Assoc -> Operator T.Text () Identity a
binary n f a = flip Infix a $ do
  pos <- withPos $ id <$ reservedOp n
  pure (f pos)

typeP' :: Parser Type'
typeP' = parens typeP
     <|> TyVar <$> tyVar
     <|> tyCon <|> unitTyCon
     <|> tyForall where
  tyCon, unitTyCon, tyForall :: Parser Type'
  tyForall = do
    reserved "forall"
    nms <- commaSep1 tyVar
    _ <- dot
    cs <- parens . commaSep1 $ typeP
    reservedOp "=>"
    TyForall nms cs <$> typeP
  tyCon = TyCon <$> name
  unitTyCon = TyCon (Name (T.pack "unit")) <$ reserved "unit"

tyVar :: Parser (Var 'ParsePhase)
tyVar = lexeme $ do
  _ <- char '\''
  x <- Tok.identStart style
  (Name . T.pack . (x:)) <$> many (Tok.identLetter style)

name :: Parser (Var 'ParsePhase)
name = Name . T.pack <$> identifier

constrName :: Parser (Var 'ParsePhase)
constrName = (Name <$> upperIdent) <?> "constructor name" where
  upperIdent = lexeme $ do
    x <- upper
    T.pack . (x:) <$> many (Tok.identLetter style)

lit :: Parser Lit
lit = intLit <|> strLit <|> true <|> false <|> unit where
  intLit = LiInt <$> natural
  strLit = LiStr . T.pack <$> stringLiteral
  true = LiBool True <$ reserved "true"
  false = LiBool False <$ reserved "false"
  unit = LiUnit <$ reserved "unit"

toplevelP :: Parser Toplevel'
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
    n <- T.pack <$> stringLiteral
    _ <- colon
    ForeignVal x n <$> typeP
  dataDecl = do
    reserved "type"
    x <- name
    xs <- many tyVar
    eq <- optionMaybe (reservedOp "=")
    case eq of
      Just _ -> do
        cs <- many $ do
          reservedOp "|"
          x <- constrName
          (,) x <$> sepBy typeP (reservedOp "*")
        pure $ TypeDecl x xs cs
      Nothing -> pure $ TypeDecl x xs []

program :: Parser [Toplevel']
program = semiSep1 toplevelP <* eof
