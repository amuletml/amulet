{-# LANGUAGE TemplateHaskellQuotes, OverloadedStrings #-}
module Language.Lua.Quote
  ( lua
  , luaStmt
  , luaStmts
  ) where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Generics

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Language.Lua.Parser.Wrapper
import Language.Lua.Parser.Parser
import Language.Lua.Syntax

import qualified Text.Pretty.Note as N

lua, luaStmt, luaStmts :: QuasiQuoter
lua = luaQuote parseExpr
luaStmt = luaQuote parseStmt
luaStmts = luaQuote parseStmts

luaQuote :: Data a => Parser a -> QuasiQuoter
luaQuote parser = QuasiQuoter
  { quoteExp = go
  , quotePat = const (fail "Patterns not supported")
  , quoteType = const (fail "Types not supported")
  , quoteDec = const (fail "Declarations not supported")
  } where
  go s = do
    loc <- TH.location
    let (line, col)  = TH.loc_start loc
        pos = SourcePos (TH.loc_filename loc) line col
        spans = [( TH.loc_filename loc
                 , T.replicate (line - 1) "\n" <> T.replicate (col - 1) " " <> T.pack s)]
    case runParser pos (L.pack s) parser of
      Right res -> antiLua res
      Left e -> fail . show . N.format (N.fileSpans spans) $ e

antiLua ::  Data a => a -> TH.Q TH.Exp
antiLua = dataToExpQ (const Nothing `extQ` antiExpr `extQ` antiStmt `extQ` antiVar `extQ` liftText)

antiExpr :: LuaExpr -> Maybe (TH.Q TH.Exp)
antiExpr (LuaQuoteE x) = Just $ TH.varE  (TH.mkName (T.unpack x))
antiExpr _ = Nothing

antiVar :: LuaVar -> Maybe (TH.Q TH.Exp)
antiVar (LuaQuoteV x) = Just $ TH.varE  (TH.mkName (T.unpack x))
antiVar _ = Nothing

antiStmt :: LuaStmt -> Maybe (TH.Q TH.Exp)
antiStmt (LuaQuoteS x) = Just $ TH.varE  (TH.mkName (T.unpack x))
antiStmt _ = Nothing

liftText :: T.Text -> Maybe (TH.Q TH.Exp)
liftText txt = Just $ TH.AppE (TH.VarE 'T.pack) <$> TH.lift (T.unpack txt)
