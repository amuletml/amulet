{-# LANGUAGE GADTs, OverloadedStrings #-}
module Test.Lua.Gen
  ( genExpr
  , genStmt
  , genStmts
  ) where

import Control.Monad.Identity

import qualified Data.Set as Set
import qualified Data.Text as T

import Language.Lua.Syntax

import qualified Hedgehog.Range as Range
import qualified Hedgehog.Gen as Gen
import Hedgehog

genIdent :: (MonadGen m, GenBase m ~ Identity) => m T.Text
genIdent = Gen.filter (`Set.notMember` keywords) $ do
  first <- Gen.element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_"
  rest <- Gen.text (Range.linear 0 25) (Gen.element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")
  pure (T.cons first rest)

genVar :: (MonadGen m, GenBase m ~ Identity) => m LuaVar
genVar =
  Gen.recursive Gen.choice
    [ genName ]
    [ LuaIndex <$> genExpr <*> genKey ]

genName :: (MonadGen m, GenBase m ~ Identity) => m LuaVar
genName = LuaName <$> genIdent

genKey :: (MonadGen m, GenBase m ~ Identity) => m LuaExpr
genKey = Gen.frequency [(3, LuaString <$> genIdent), (1, genExpr)]

genExpr :: (MonadGen m, GenBase m ~ Identity) => m LuaExpr
genExpr =
  Gen.recursive Gen.choice
  [ pure LuaNil, pure LuaTrue, pure LuaFalse, pure LuaDots
  , LuaNumber . (/2^(3::Int)) . fromInteger <$> Gen.integral (Range.exponential 0 (2^dR))
  , LuaInteger <$> Gen.int (Range.exponential 0 (2^iR))
  , LuaString <$> Gen.text (Range.linear 0 200) Gen.lower -- TODO Gen.ascii
  , LuaRef <$> genName
  ]
  [ LuaCallE <$> genCall
  , LuaRef <$> genVar
  , LuaFunction <$> Gen.list (Range.linear 0 5) genName <*>  genStmts
  , LuaTable <$> Gen.list (Range.linear 0 15) ((,) <$> genKey <*> genExpr)
  , LuaBinOp <$> genExpr <*> genBin <*> genExpr
  , LuaUnOp <$> genUn <*> genExpr
  ]

  where
    dR , iR :: Int
    dR = 10
    iR = 24

    genBin = Gen.element
      [ "+", "-", "*", "/", "%", "^", "..", "==", "~=", ">", "<", ">=", "<="
      , "and", "or" ]
    genUn = Gen.element ["-", "not"]

genCall :: (MonadGen m, GenBase m ~ Identity) => m LuaCall
genCall = Gen.frequency
  [(5, LuaCall <$> genExpr <*> Gen.list (Range.linear 0 5) genExpr)
  ,(1, LuaInvoke <$> genExpr <*> genIdent <*> Gen.list (Range.linear 0 5) genExpr)
  ]

genStmts :: (MonadGen m, GenBase m ~ Identity) => m [LuaStmt]
genStmts =
  (++) <$> Gen.list (Range.linear 0 10) genStmt
       <*> Gen.frequency
           [ (5, pure [])
           , (1, pure . LuaReturn <$> Gen.list (Range.linear 0 5) genExpr)
           , (1, pure [LuaBreak])
           ]

genStmt :: (MonadGen m, GenBase m ~ Identity) => m LuaStmt
genStmt =
  Gen.recursive Gen.choice
  [  ]
  [ LuaDo <$> genStmts
  , LuaAssign <$> Gen.list (Range.linear 1 5) genVar <*> Gen.list (Range.linear 1 5) genExpr
  , LuaWhile <$> genExpr <*> genStmts
  , LuaRepeat <$> genStmts <*> genExpr
  , LuaFornum <$> genName <*> genExpr <*> genExpr <*> genCounter <*> genStmts
  , LuaFor <$> Gen.list (Range.linear 1 5) genName <*> Gen.list (Range.linear 1 5) genExpr <*> genStmts
  , LuaLocal <$> Gen.list (Range.linear 1 5) genName <*> Gen.list (Range.linear 0 5) genExpr
  , LuaLocalFun <$> genName <*> Gen.list (Range.linear 0 5) genName <*> genStmts
  , LuaIfElse <$> Gen.list (Range.linear 1 5) ((,) <$> genElseExpr <*> genStmts)
  ]

  where
    genElseExpr = Gen.frequency [(3, genExpr), (1, pure LuaTrue)]
    genCounter = Gen.frequency [(3, pure (LuaInteger 1)), (1, genExpr)]
