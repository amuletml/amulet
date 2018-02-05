{-# LANGUAGE TupleSections, TypeFamilies, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Parser where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.Expr
import Text.Parsec
import Data.Spanned
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
type Constructor' = Constructor Parsed

bindGroup :: Parser [(Var Parsed, Expr', Span)]
bindGroup = sepBy1 decl (reserved "and") where
  decl = do
    x <- name
    ps <- many (patternP parens)
    tp <- optionMaybe (colon *> typeP)
    reservedOp "="
    bd <- case tp of
      Nothing -> exprP
      Just t -> withPos (Ascription <$> exprP <*> pure t)
    withPos $ case ps of
      [] -> pure (x, bd,)
      _ -> do
        let fun' pt b = Fun pt b (annotation bd)
        pure (x, (foldr fun' bd ps),)

withPos :: Parser (Span -> a) -> Parser a
withPos k = do
  begin <- getPosition
  k' <- k
  end <- getPosition
  case mkSpan begin end of
    Nothing -> mzero <?> (sourceName begin ++ " to be the same file as " ++ sourceName end)
    Just vl -> pure (k' vl)

exprP' :: Parser Expr'
exprP' = try access
     <|> accessSect
     <|> try eot
     <|> try rightSect
     <|> try leftSect
     <|> try bothSect
     <|> tuple
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
    x <- patternP parens
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
    p <- patternP id
    reservedOp "->"
    (,) p <$> exprP
  recIns = withPos . braces $ do
    x <- exprP
    reserved "with"
    RecordExt x <$> commaSep1 row
  rec = withPos $
          Record <$> braces (commaSep row)
  row = do
    x <- identifier
    reservedOp "="
    (T.pack x, ) <$> exprP
  access = withPos . lexeme $ do
    (rec :: Expr Parsed) <- (parens exprP <* char '.') <|> withPos (do
      x <- Tok.identStart style
      xs <- many (Tok.identLetter style)
      '.' <- char '.'
      pure . VarRef . Name . T.pack $ (x:xs))
    (key :: T.Text) <- do
      x <- Tok.identStart style
      T.pack . (x:) <$> many (Tok.identLetter style)
    pure $ Access rec key
  accessSect = withPos . lexeme $ do
    '.' <- char '.'
    x <- Tok.identStart style
    AccessSection . T.pack . (x:) <$> many (Tok.identLetter style)
  rightSect, leftSect, bothSect :: Parser Expr'
  bothSect = withPos . parens . lexeme $ do
    op <- withPos $ do
      actual <- operator'
      pure (VarRef (Name (T.pack actual)))
    pure (BothSection op)
  leftSect = withPos . parens $ do
    op <- withPos $ do
      actual <- operator'
      pure (VarRef (Name (T.pack actual)))
    x <- exprP'
    pure (LeftSection op x)
  rightSect = withPos . parens $ do
    x <- exprP'
    op <- withPos $ do
      actual <- operator'
      pure (VarRef (Name (T.pack actual)))
    pure (RightSection x op)
  eot = withPos . parens $ do
    x <- exprP
    reservedOp ":"
    Ascription x <$> typeP
  tuple = withPos . parens $ do
    x <- commaSep exprP
    pure $ case x of
      [] -> Literal LiUnit
      [x] -> const x
      l -> Tuple l

operator' :: Parser String
operator' = lexeme ((:) <$> Tok.opStart style <*> many (Tok.opLetter style))

patternP :: (forall a. Parser a -> Parser a) -> Parser Pattern'
patternP cont = wildcard
            <|> capture
            <|> try pType
            <|> try destructure
            <|> tuple
            <|> record where
  wildcard, destructure, pType, capture, record :: Parser Pattern'
  wildcard = withPos (Wildcard <$ reservedOp "_") <?> "wildcard pattern"
  capture = withPos (Capture <$> varName) <?> "capturing pattern"
  varName = (Name <$> lowerIdent) <?> "variable name"
  destructure = (<?> "destructuring") . withPos . cont $ do
    ps <- constrName
    Destructure ps <$> optionMaybe (patternP id)
  lowerIdent = lexeme $ do
    x <- lower
    T.pack . (x:) <$> many (Tok.identLetter style)
  pType = (<?> "typed pattern") . withPos . parens $ do
    x <- patternP id
    reservedOp ":"
    PType x <$> typeP
  record = withPos . braces $ do
    rows <- commaSep1 $ do
      x <- identifier
      reservedOp "="
      (T.pack x,) <$> patternP id
    pure $ PRecord rows
  tuple = (<?> "tuple pattern") . withPos . parens $ do
    x <- commaSep1 (patternP id)
    pure $ case x of
      [] -> error "impossible; commaSep*1*"
      [x] -> const x
      xs -> PTuple xs

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
    (hd, tl) <- do
      x <- typeP'
      y' <- many typeP'
      pure (x, y')
    pure $ foldl TyApp hd tl

  table :: [[ Operator T.Text () Identity (Type Parsed) ]]
  table = [ [ binary "->" (const TyArr)   AssocRight ]
          , [ binary "*"  (const TyTuple) AssocRight ] ]

binary :: String -> (Span -> a -> a -> a) -> Assoc -> Operator T.Text () Identity a
binary n f a = flip Infix a $ do
  pos <- withPos $ id <$ reservedOp n
  pure (f pos)

typeP' :: Parser Type'
typeP' = parens typeP
     <|> (TyVar <$> tyVar)
     <|> tyCon <|> unitTyCon
     <|> tyForall
     <|> try openRec
     <|> closedRec where
  tyCon, unitTyCon, tyForall :: Parser Type'
  tyForall = do
    reserved "forall" <|> reservedOp "∀"
    nms <- many1 tyVar
    _ <- dot
    TyForall nms <$> typeP
  tyCon = TyCon <$> name
  unitTyCon = TyCon (Name (T.pack "unit")) <$ reserved "unit"
  openRec = braces $ do
    x <- typeP
    reservedOp "|"
    TyRows x <$> commaSep1 tyRow
  closedRec = braces $ TyExactRows <$> commaSep1 tyRow
  tyRow = do
    x <- identifier
    reservedOp ":"
    (T.pack x,) <$> typeP

tyVar :: Parser (Var Parsed)
tyVar = lexeme $ do
  _ <- char '\''
  x <- Tok.identStart style
  Name . T.pack . (x:) <$> many (Tok.identLetter style)

name :: Parser (Var Parsed)
name = Name . T.pack <$> identifier

constrName :: Parser (Var Parsed)
constrName = (Name <$> upperIdent) <?> "constructor name" where
  upperIdent = lexeme $ do
    x <- upper
    T.pack . (x:) <$> many (Tok.identLetter style)

lit :: Parser Lit
lit = intLit <|> strLit <|> true <|> false where
  intLit = LiInt <$> natural
  strLit = LiStr . T.pack <$> stringLiteral
  true = LiBool True <$ reserved "true"
  false = LiBool False <$ reserved "false"

toplevelP :: Parser Toplevel'
toplevelP = letStmt <|> try foreignVal <|> dataDecl where
  letStmt = do
    reserved "let"
    LetStmt <$> bindGroup
  foreignVal = withPos $ do
    reserved "external"
    reserved "val"
    x <- name
    reservedOp ":"
    ty <- typeP
    reservedOp "="
    n <- T.pack <$> stringLiteral
    pure $ ForeignVal x n ty
  dataDecl = do
    reserved "type"
    x <- name
    xs <- many tyVar
    eq <- optionMaybe (reservedOp "=")
    case eq of
      Just _ -> do
        first <- optionMaybe constructor
        cs <- many $ do
          reservedOp "|"
          constructor
        pure $ TypeDecl x xs (maybe cs (:cs) first)
      Nothing -> pure $ TypeDecl x xs []

constructor :: Parser Constructor'
constructor = try arg <|> unit where
  unit = withPos (UnitCon <$> constrName)
  arg = withPos $ do
    nm <- constrName
    reserved "of"
    ArgCon nm <$> typeP


program :: Parser [Toplevel']
program = Tok.whiteSpace lexer *> toplevelP `sepBy1` toplevelSep <* eof where
  toplevelSep = lexeme (char ';' *> char ';') <?> "top-level statement separator"
