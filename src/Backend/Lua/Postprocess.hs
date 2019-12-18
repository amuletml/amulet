{-|Handles post-processing the generated Lua sources, injecting any
 additional code which may be needed.

 We only emit some definitions when they are required by the rest of the
 program, rather than unconditionally emitting them. This prevents us
 from emitting 20+ functions for every builtin definition.

 In order to do this, we build a map of variables -> definitions, and run
 a separate pass after generation to add those definitions in if
 required.

 For now, we optionally emit some foreign declarations, constructors and
 all builtins, as their usages may be elided during code generation. Any
 other definition will have already been stripped by the optimiser, and
 so it is not safe to remove them.
-}
module Backend.Lua.Postprocess
  ( addBuiltins
  , addExport
  ) where

import Control.Lens

import qualified Data.VarSet as VarSet
import qualified Data.VarMap as VarMap
import qualified Data.Sequence as Seq
import qualified Data.Map as Map
import Data.Sequence (Seq)
import Data.Tuple

import Language.Lua.Syntax

import Backend.Lua.Builtin (ExtraVars)
import Backend.Lua.Emit
import Backend.Escape

import Core.Var

import Syntax.Resolve.Scope
import Syntax.Var

-- | Walks the Lua tree and identifies which builtins are not fully
-- applied (and so their variable is referenced), injecting them into the
-- program.
addBuiltins :: Seq LuaStmt -> TopEmitState -> (Seq LuaStmt, TopEmitState)
addBuiltins stmt state = fmap (flip (set topExVars) state) . swap
                       $ genBuiltins (VarSet.toList (foldMap opsStmt stmt)) (state ^. topExVars) stmt
  where
    genBuiltins :: [CoVar] -> ExtraVars -> Seq LuaStmt -> (ExtraVars, Seq LuaStmt)
    genBuiltins [] vs ss = (vs, ss)
    genBuiltins (b:bs) vs ss = case VarMap.lookup b vs of
      Nothing -> genBuiltins bs vs ss
      Just (deps, stmts) -> genBuiltins (deps ++ bs) (VarMap.delete b vs) (stmts <> ss)

    opsStmt :: LuaStmt -> VarSet.Set
    opsStmt (LuaDo b) = foldMap opsStmt b
    opsStmt (LuaAssign vs xs) = foldMap opsVar vs <> foldMap opsExpr xs
    opsStmt (LuaWhile t b) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaRepeat b t) = opsExpr t <> foldMap opsStmt b
    opsStmt (LuaFornum _ b e s bo) = opsExpr b <> opsExpr e <> opsExpr s <> foldMap opsStmt bo
    opsStmt (LuaFor _ g b) = foldMap opsExpr g <> foldMap opsStmt b
    opsStmt (LuaLocal _ e) = foldMap opsExpr e
    opsStmt (LuaLocalFun _ _ b) = foldMap opsStmt b
    opsStmt (LuaReturn r) = foldMap opsExpr r
    opsStmt (LuaIfElse t) = foldMap (\(c, b) -> opsExpr c <> foldMap opsStmt b) t
    opsStmt LuaBreak = mempty
    opsStmt (LuaCallS f) = opsCall f
    opsStmt LuaQuoteS{} = mempty
    opsStmt LuaRawS{} = mempty

    opsExpr :: LuaExpr -> VarSet.Set
    opsExpr LuaNil = mempty
    opsExpr LuaTrue = mempty
    opsExpr LuaFalse = mempty
    opsExpr LuaDots = mempty
    opsExpr LuaNumber{} = mempty
    opsExpr LuaInteger{} = mempty
    opsExpr LuaString{} = mempty
    opsExpr LuaQuoteE{} = mempty
    opsExpr LuaBitE{} = mempty

    opsExpr (LuaCallE f) = opsCall f
    opsExpr (LuaRef v) = opsVar v
    opsExpr (LuaFunction _ b) = foldMap opsStmt b
    opsExpr (LuaTable ks) = foldMap (\(k, v) -> opsExpr k <> opsExpr v) ks
    opsExpr (LuaBinOp l _ r) = opsExpr l <> opsExpr r
    opsExpr (LuaUnOp _ x) = opsExpr x

    opsVar :: LuaVar -> VarSet.Set
    opsVar (LuaName t) = foldMap VarSet.singleton (Map.lookup t usedNames)
    opsVar (LuaIndex t k) = opsExpr t <> opsExpr k
    opsVar LuaQuoteV{} = mempty

    opsCall (LuaCall f xs) = foldMap opsExpr (f:xs)
    opsCall (LuaInvoke f _ xs) = foldMap opsExpr (f:xs)

    usedNames =  Map.filter (`VarMap.member` (state ^. topExVars)) (fromEsc (state ^. topEscape))

addExport :: Signature -> Seq LuaStmt -> TopEmitState -> Seq LuaStmt
addExport sig stmt state = stmt Seq.|> LuaReturn [exports sig] where
  var :: CoVar -> LuaExpr
  var v =
    case state ^. topVars . at v of
      Just (_, x:_) -> unsimple x
      _ -> LuaNil

  exports :: Signature -> LuaExpr
  exports sig = LuaTable $
    let vars = Map.foldrWithKey
                 (\k s xs ->
                    case s of
                      SVar (TgName n i) -> (LuaString k, var (CoVar i (Just n) ValueVar)) : xs
                      _ -> xs)
                 [] (sig ^. vals)
        mods = Map.foldrWithKey (\k (_, Just m) xs -> (LuaString k, exports m) : xs) vars (sig ^. modules)
    in mods
