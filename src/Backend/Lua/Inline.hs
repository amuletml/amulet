{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Backend.Lua.Inline
  ( shouldInline
  , substExpr
  , substStmt
  ) where

import Control.Monad.State.Strict
import Control.Applicative
import Control.Lens

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Foldable
import Data.Monoid

import Language.Lua.Syntax

data InlineState
  = IS { _pendingVars :: [LuaVar]
       , _skippedVars :: [LuaVar]
       }

makeLenses ''InlineState

threshold :: Int
threshold = 10

-- | Determine if an expression should be inlined
--
-- We require the following to hold for an expression to be inlinable:
--
--  - Every argument is used exactly once, and in the order that it was
--    passed. This requirement is a little harsh, but our current
--    handling of unboxed tuples necessitates it.
--
--  - The entire body has a "size" less than or equal to the 'threshold'.
shouldInline :: [LuaVar] -> [LuaStmt] -> Bool
shouldInline args stmts =
  case runStateT (foldMapM sizeStmt stmts) (IS args mempty) of
    Just (Sum n, IS [] []) | n <= threshold -> True
    _ -> False

  where
  sizeStmt :: ( MonadState InlineState m
              , Alternative m )
           => LuaStmt -> m (Sum Int)
  sizeStmt (LuaReturn r) = foldMapM sizeExpr r
  sizeStmt (LuaCallS c) = sizeCall c
  sizeStmt (LuaAssign as vs) = (<>) <$> foldMapM sizeAssign as <*> foldMapM sizeExpr vs
  -- For now we'll just skip these
  sizeStmt _ = empty

  -- We skip assignments to arguments, otherwise all is good
  sizeAssign v@LuaName{} | Set.member v allVars = empty
  sizeAssign a = sizeExpr (LuaRef a)

  sizeExpr :: ( MonadState InlineState m
              , Alternative m )
           => LuaExpr -> m (Sum Int)
  -- Basic literals
  sizeExpr LuaNil = k 1
  sizeExpr LuaTrue = k 1
  sizeExpr LuaFalse = k 1
  sizeExpr LuaDots = empty
  sizeExpr LuaNumber{} = k 1
  sizeExpr LuaInteger{} = k 1
  sizeExpr (LuaString s)
    | T.length s <= 16 = k 2
    | otherwise = empty
  -- Just the sum of their parts plus some extra constant
  sizeExpr (LuaBinOp l _ r) = ex 1 $ (<>) <$> sizeExpr l <*> sizeExpr r
  sizeExpr (LuaUnOp _ o) = ex 1 $ sizeExpr o
  sizeExpr (LuaTable fs) = ex 2 $ foldMapM (\(a, b) -> (<>) <$> sizeExpr a <*> sizeExpr b) fs
  sizeExpr (LuaCallE c) = sizeCall c
  sizeExpr (LuaRef (LuaIndex t f)) = ex 1 $ (<>) <$> sizeExpr t <*> sizeExpr f

  sizeExpr (LuaRef v@LuaName{})
    -- References to non-argument variables are fine.
    | Set.notMember v allVars = k 1
    | otherwise = do
        vs <- use pendingVars
        case span (/=v) vs of
          -- The variable has already been seen, so we can't inline
          (_, []) -> empty
          -- Otherwise, extend the unnused set and pop the pending one.
          (skipped, _:pending) -> do
            pendingVars .= pending
            skippedVars %= (skipped++)
            k 0

  -- Skip functions, as they mess with scoping
  sizeExpr LuaFunction{} = empty
  -- Impossible!
  sizeExpr LuaQuoteE{} = empty
  sizeExpr LuaBitE{} = empty
  sizeExpr (LuaRef LuaQuoteV{}) = empty

  sizeCall (LuaCall f as) = ex 2 $ foldMapM sizeExpr (f:as)
  sizeCall (LuaInvoke t _ as) = ex 3 $ foldMapM sizeExpr (t:as)

  k :: Applicative m => Int -> m (Sum Int)
  k = pure . Sum

  ex :: Functor f => Int -> f (Sum Int) -> f (Sum Int)
  ex a b = (Sum a<>) <$> b

  foldMapM f = foldrM (\a b -> (<>b) <$> f a) mempty

  allVars :: Set.Set LuaVar
  allVars = Set.fromList args

-- | Substitute a series of variables within a Lua statement.
substStmt :: Map.Map LuaVar LuaExpr -> LuaStmt -> LuaStmt
substStmt s = go where
  go (LuaReturn rs) = LuaReturn (map goE rs)
  go (LuaCallS c) = LuaCallS (substCall s c)
  go (LuaAssign as rs) = LuaAssign (map (substVar s) as) (map goE rs)
  go _ = error "substStmt not defined for this type"

  goE = substExpr s

substVar :: Map.Map LuaVar LuaExpr -> LuaVar -> LuaVar
substVar _ v@LuaName{} = v
substVar _ v@LuaQuoteV{} = v
substVar s (LuaIndex t f) = LuaIndex (substExpr s t) (substExpr s f)

-- | Substitute a series of variables within a Lua expression.
substExpr :: Map.Map LuaVar LuaExpr -> LuaExpr -> LuaExpr
substExpr s = go where
  go n@(LuaRef v@LuaName{}) = Map.findWithDefault n v s
  go (LuaRef r) = LuaRef (substVar s r)

  go n@LuaNil = n
  go n@LuaTrue = n
  go n@LuaFalse = n
  go n@LuaDots = n
  go n@LuaNumber{} = n
  go n@LuaInteger{} = n
  go n@LuaString{} = n
  go n@LuaBitE{} = n
  go n@LuaQuoteE{} = n

  go (LuaTable fs) = LuaTable $ map (bimap go go) fs
  go (LuaBinOp l o r) = LuaBinOp (go l) o (go r)
  go (LuaUnOp o r) = LuaUnOp o (go r)
  go (LuaCallE c) = LuaCallE (substCall s c)
  go (LuaFunction as ss) =
    let s' = foldr Map.delete s as
    in LuaFunction as (map (substStmt s') ss)

substCall :: Map.Map LuaVar LuaExpr -> LuaCall -> LuaCall
substCall s (LuaCall f as) = LuaCall (substExpr s f) (map (substExpr s) as)
substCall s (LuaInvoke t f as) = LuaInvoke (substExpr s t) f (map (substExpr s) as)
