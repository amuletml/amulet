{-# LANGUAGE FlexibleContexts, NamedFieldPuns #-}
module Errors where

import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as T
import qualified Data.Text as T
import Data.Position

import Text.Pretty.Semantic
import qualified Text.Pretty.Ansi as A
import qualified Text.Pretty.Note as N

import Parser.Wrapper
import Parser.Lexer
import Parser.Token

reportS :: N.Note a Style => a -> [(SourceName, T.Text)] -> IO ()
reportS err fs = T.putStrLn
                 . A.displayDecorated
                 . fmap (either N.toAnsi toAnsi)
                 . filterSimpleDoc (either (const True) uncommentFilter)
                 . renderPretty 0.4 100
                 . N.format (N.fileSpans fs highlightAmulet) $ err

highlightAmulet :: T.Text -> N.Highlighted Style
highlightAmulet t =
  case runLexer "" (L.fromStrict t) lexerScan of
    (Nothing, _) -> N.defaultHighlight t
    (Just ts, _) -> buildTokens 0 ts

  where
    buildTokens p [] = [(text, T.drop p t)]
    buildTokens p (Token kind s e:ts) =
      let s' = spCol s - 1
          e' = spCol e
      in (text, T.take (s' - p) . T.drop p $ t)
       : (fmap Right . toStyle kind . text, T.take (e' - s') . T.drop s' $ t)
       : buildTokens e' ts

    toStyle TcIdentifier{} = id
    toStyle TcOpIdent{} = soperator
    toStyle TcConIdent{} = stypeCon

    toStyle TcIdentifierQual{} = id
    toStyle TcOpIdentQual{} = soperator
    toStyle TcConIdentQual{} = stypeCon

    toStyle TcDotQual{} = id
    toStyle TcOp{} = soperator
    toStyle TcTyvar{} = stypeVar
    toStyle TcAccess{} = id
    toStyle TcHole{} = id

    toStyle TcString{} = sstring
    toStyle TcFloat{} = sliteral
    toStyle TcInteger{} = sliteral

    toStyle TcComment{} = scomment
    toStyle TcWhitespace{} = id

    toStyle t
      | t >= TcArrow && t <= TcUnderscore = soperator
      | t >= TcComma && t <= TcCSquare = soperator
      | t >= TcLet && t <= TcAs = soperator
      | otherwise = id
