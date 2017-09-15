module Parser where

import qualified Text.Parsec.Token as Tok
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec

import Parser.Lexer

import Data.Functor.Identity

import Control.Monad
import Syntax

data BeginStmt
  = BeginLet [(Var, Expr)]
  | BeginRun Expr
  deriving (Eq)

bindGroup :: Parser [(Var, Expr)]
bindGroup = sepBy1 decl (reserved "and") where
  decl = do
    x <- name
    ps <- many patternP
    reservedOp "="
    bd@(start,end,_) <- exprP
    case ps of
      [] -> pure (x, bd)
      _ -> do
        let fun' pt b = (start, end, Fun pt b)
        pure (x, foldr fun' bd ps)

withPos :: Parser a -> Parser (SourcePos, SourcePos, a)
withPos k = do
  begin <- getPosition
  vl <- k
  end <- getPosition
  pure (begin, end, vl)

exprP' :: Parser Expr
exprP' = parens exprP
     <|> funExpr
     <|> letExpr
     <|> ifExpr
     <|> multiwayIfExpr
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

multiwayIfExpr :: Parser Expr
multiwayIfExpr = withPos $ do
  reserved "if"
  as <- many1 arm
  pure $ MultiWayIf as
  where
    arm = do
      reservedOp "|"
      g <- exprP
      reservedOp "->"
      (,) <$> pure g <*> exprP

patternP :: Parser Pattern
patternP = wildcard <|> capture <|> constructor <|> try pType <|> destructure where
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
  pType = parens $ do
    x <- patternP
    reservedOp ":"
    PType x <$> typeP

exprP :: Parser Expr
exprP = exprOpP where
  expr' = do
    (start, end, (hd, tl)) <- withPos $ do
      x <- exprP'
      y' <- many exprP'
      pure (x, y')
    let app' a b = (start, end, App a b)
    pure $ foldl app' hd tl
  exprOpP = buildExpressionParser table expr' <?> "expression"
  bop x = binary x (\(s, e) a b -> (s, e, BinOp a (s, e, (VarRef (Name x))) b))
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
  table = [ [ binary "->" (const TyArr) AssocRight ]]

binary :: String -> ((SourcePos, SourcePos) -> a -> a -> a) -> Assoc -> Operator String () Identity a
binary n f a = flip Infix a $ do
  (start, end, _) <- withPos $ reservedOp n
  pure (f (start, end))

typeP' :: Parser Type
typeP' = parens typeP
     <|> TyVar <$> tyVar
     <|> tyCon <|> unitTyCon
     <|> tyForall where
  tyForall = do
    reserved "forall"
    nms <- commaSep1 tyVar
    _ <- dot
    cs <- parens . commaSep1 $ typeP
    reservedOp "=>"
    TyForall nms cs <$> typeP
  tyCon = TyCon <$> name
  unitTyCon = TyCon (Name "unit") <$ reserved "unit"

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
lit = intLit <|> strLit <|> true <|> false <|> unit where
  intLit = LiInt <$> natural
  strLit = LiStr <$> stringLiteral
  true = LiBool True <$ reserved "true"
  false = LiBool False <$ reserved "false"
  unit = LiUnit <$ reserved "unit"

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
    eq <- optionMaybe (reservedOp "=")
    case eq of
      Just _ -> do
        cs <- many $ do
          reservedOp "|"
          x <- constrName
          (,) x <$> sepBy typeP (reservedOp "*")
        pure $ TypeDecl x xs cs
      Nothing -> pure $ TypeDecl x xs []

program :: Parser [Toplevel]
program = semiSep1 toplevelP <* eof
