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

type Expr' = Expr Parsed
type Toplevel' = Toplevel Parsed
type Type' = Type Parsed
type Pattern' = Pattern Parsed

data BeginStmt
  = BeginLet [(Var Parsed, Expr')]
  | BeginRun Expr'
  deriving (Eq)

bindGroup :: Parser [(Var Parsed, Expr')]
bindGroup = sepBy1 decl (reserved "and") where
  decl = do
    x <- name
    ps <- many patternP
    reservedOp "="
    bd <- exprP
    case ps of
      [] -> pure (x, bd)
      _ -> do
        let fun' pt b = Fun pt b (annotation bd)
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
     <|> withPos (Hole <$> hole)
     <|> withPos (Literal <$> lit)
     <|> try recIns
     <|> rec where
  hole = lexeme $ do
    '_' <- char '_'
    x <- optionMaybe lower
    case x of
      Just x' -> Name . T.pack . (x':) <$> many (Tok.identLetter style)
      Nothing -> pure . Name . T.pack $ "_"
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
      (Destructure v _ p:xs) -> do
        reservedOp "->"
        (,) (Destructure v xs p) <$> exprP
      _ -> mzero
  recIns = withPos . braces $ do
    x <- exprP
    reserved "with"
    RecordExt x <$> many1 row
  rec = withPos $
          Record <$> braces (many row)
  row = do
    x <- name
    reservedOp "="
    (x, ) <$> exprP


patternP :: Parser Pattern'
patternP = wildcard
       <|> capture
       <|> constructor
       <|> try pType
       <|> destructure
       <|> record where
  wildcard, constructor, destructure, pType, capture, record :: Parser Pattern'
  wildcard = withPos (Wildcard <$ reservedOp "_")
  capture = withPos (Capture <$> varName)
  varName = (Name <$> lowerIdent) <?> "variable name"
  constructor = withPos (flip Destructure [] <$> constrName)
  destructure = withPos . parens $ do
    ps <- constrName
    Destructure ps <$> many1 patternP
  lowerIdent = lexeme $ do
    x <- lower
    T.pack . (x:) <$> many (Tok.identLetter style)
  pType = withPos . parens $ do
    x <- patternP
    reservedOp ":"
    PType x <$> typeP
  record = withPos . braces $ do
    rows <- many $ do
      x <- name
      reservedOp "="
      (x,) <$> patternP
    pure $ PRecord rows

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
  bop x = binary x (\p a b -> BinOp a (VarRef (Name (T.pack x)) p) b p :: Expr Parsed)
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
  type' = do
    (hd, tl, pos) <- withPos $ do
      x <- typeP'
      y' <- many typeP'
      pure (x, y',)
    let app' a b = TyApp a b pos
    pure $ foldl app' hd tl

  table :: [[ Operator T.Text () Identity (Type Parsed) ]]
  table = [ [ binary "->" (\p a b -> TyArr a b p) AssocRight ]]

binary :: String -> (Span -> a -> a -> a) -> Assoc -> Operator T.Text () Identity a
binary n f a = flip Infix a $ do
  pos <- withPos $ id <$ reservedOp n
  pure (f pos)

typeP' :: Parser Type'
typeP' = parens typeP
     <|> withPos (TyVar <$> tyVar)
     <|> tyCon <|> unitTyCon
     <|> tyForall where
  tyCon, unitTyCon, tyForall :: Parser Type'
  tyForall = withPos $ do
    reserved "forall"
    nms <- commaSep1 tyVar
    _ <- dot
    cs <- parens . commaSep1 $ typeP
    reservedOp "=>"
    TyForall nms cs <$> typeP
  tyCon = withPos (TyCon <$> name)
  unitTyCon = withPos (TyCon (Name (T.pack "unit")) <$ reserved "unit")

tyVar :: Parser (Var Parsed)
tyVar = lexeme $ do
  _ <- char '\''
  x <- Tok.identStart style
  (Name . T.pack . (x:)) <$> many (Tok.identLetter style)

name :: Parser (Var Parsed)
name = Name . T.pack <$> identifier

constrName :: Parser (Var Parsed)
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
  letStmt = withPos $ do
    reserved "let"
    LetStmt <$> bindGroup
  valStmt = withPos $ do
    reserved "val"
    x <- name
    _ <- colon
    ValStmt x <$> typeP
  foreignVal = withPos $ do
    reserved "val"
    reserved "foreign"
    x <- name
    n <- T.pack <$> stringLiteral
    _ <- colon
    ForeignVal x n <$> typeP
  dataDecl = withPos $ do
    reserved "type"
    x <- name
    xs <- many tyVar
    eq <- optionMaybe (reservedOp "=")
    case eq of
      Just _ -> do
        first <- optionMaybe $ do
          x <- constrName
          (,) x <$> sepBy typeP (reservedOp "*")
        cs <- many $ do
          reservedOp "|"
          x <- constrName
          (,) x <$> sepBy typeP (reservedOp "*")
        pure $ TypeDecl x xs (maybe cs (:cs) first)
      Nothing -> pure $ TypeDecl x xs []

program :: Parser [Toplevel']
program = spaces *> semiSep1 toplevelP <* eof
