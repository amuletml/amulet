module Language.Lua.Parser
  ( ParseError(..)
  , SourcePos(..)

  , parseExpr
  , parseStmt
  , parseStmts
  , highlightLua
  )
  where

import qualified Data.Text.Lazy as L
import qualified Data.Text as T

import Text.Pretty.Semantic
import Text.Pretty.Note

import qualified Language.Lua.Parser.Parser as P
import Language.Lua.Parser.Wrapper
import Language.Lua.Parser.Token
import Language.Lua.Parser.Lexer
import Language.Lua.Parser.Error
import Language.Lua.Syntax

parseExpr :: SourcePos -> L.Text -> Either ParseError LuaExpr
parseExpr p i = runParser p i P.parseExpr

parseStmt :: SourcePos -> L.Text -> Either ParseError LuaStmt
parseStmt p i = runParser p i P.parseStmt

parseStmts :: SourcePos -> L.Text -> Either ParseError [LuaStmt]
parseStmts p i = runParser p i P.parseStmts

highlightLua :: T.Text -> Highlighted Style
highlightLua t =
  case runParser (SourcePos "" 0 0) (L.fromStrict t) lexer of
    Left _ -> defaultHighlight t
    Right ts -> buildTokens 0 ts

  where
    buildTokens p [] = [(text, T.drop p t)]
    buildTokens p (Token kind s e:ts) =
      let s' = spCol s
          e' = spCol e + 1
      in (text, T.take (s' - p) . T.drop p $ t)
       : (fmap Right . toStyle kind . text, T.take (e' - s') . T.drop s' $ t)
       : buildTokens e' ts

    toStyle TcString{} = sstring
    toStyle TcFloat{} = sliteral
    toStyle TcInteger{} = sliteral
    toStyle TcComment{} = scomment
    toStyle TcWhitespace{} = id
    toStyle TcQuoteE{} = stypeVar
    toStyle TcQuoteS{} = stypeVar
    toStyle TcQuoteV{} = stypeVar
    toStyle t
      | t >= TcAnd && t <= TcWhile = skeyword
      | t >= TcColon && t <= TcLen = soperator
      | otherwise = id

    lexer = do
      t <- lexerScan
      case t of
        Token TcEOF _ _ -> pure [t]
        _ -> (t:) <$> lexer
