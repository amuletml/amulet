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
  { quoteExp = go (dataToExpQ (antis TH.varE liftTE))
  , quotePat = go (dataToPatQ (antis TH.varP liftTP))
  , quoteType = const (fail "Types not supported")
  , quoteDec = const (fail "Declarations not supported")
  } where

  go build s = do
    loc <- TH.location
    let (line, col)  = TH.loc_start loc
        pos = SourcePos (TH.loc_filename loc) line col
        spans = [( TH.loc_filename loc
                 , T.replicate (line - 1) "\n" <> T.replicate (col - 1) " " <> T.pack s)]
    case runParser pos (L.pack s) parser of
      Right res -> build res
      Left e -> fail . show . N.format (N.fileSpans spans) $ e

  antis var other =
    const Nothing
    `extQ` antiExpr var
    `extQ` antiStmt var
    `extQ` antiVar var
    `extQ` other

  liftTE :: T.Text -> Maybe (TH.Q TH.Exp)
  liftTE = Just . liftText

  liftTP :: T.Text -> Maybe (TH.Q TH.Pat)
  liftTP txt = Just $ do
    txt' <- liftText txt
    pure (TH.ViewP (TH.AppE (TH.VarE '(==)) txt') (TH.ConP 'True []))

antiExpr :: (TH.Name -> TH.Q a) -> LuaExpr -> Maybe (TH.Q a)
antiExpr var (LuaQuoteE x) = Just $ var . TH.mkName . T.unpack $ x
antiExpr _ _ = Nothing

antiVar :: (TH.Name -> TH.Q a) -> LuaVar -> Maybe (TH.Q a)
antiVar var (LuaQuoteV x) = Just $ var . TH.mkName . T.unpack $ x
antiVar _ _ = Nothing

antiStmt :: (TH.Name -> TH.Q a) -> LuaStmt -> Maybe (TH.Q a)
antiStmt var (LuaQuoteS x) = Just . var . TH.mkName . T.unpack $ x
antiStmt _ _ = Nothing

liftText :: T.Text -> TH.Q TH.Exp
liftText txt = TH.AppE (TH.VarE 'T.pack) <$> TH.lift (T.unpack txt)
