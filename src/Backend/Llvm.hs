{-# LANGUAGE OverloadedStrings, FlexibleContexts, ConstraintKinds,
   DisambiguateRecordFields, ViewPatterns #-}
module Backend.Llvm (compileToLlvm) where

import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Lens

import Syntax.Resolve.Scope (Signature(..))

import Backend.Llvm.Escape

import qualified Data.Text.Encoding as T
import qualified Data.ByteString as Bs
import qualified Data.VarMap as Vm
import qualified Data.Text as T

import Data.VarMap (Map)

import Core.Types
import Core.Core
import Core.Var

import qualified LLVM.AST.Constant as Llvm
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Float as Float
import qualified LLVM.AST.Type as Ty
import qualified LLVM.AST as Llvm

import LLVM.IRBuilder

type MonadGen m = ( MonadModuleBuilder m
                  , MonadState EscapeScope m
                  , MonadReader (Map Llvm.Operand) m
                  )

compileToLlvm :: Maybe Signature -> [Stmt CoVar] -> Llvm.Module
compileToLlvm _ prog = runReader (evalStateT (buildModuleT "amulet" (genStmt prog)) emptyScope) mempty

anything :: Llvm.Type
anything = Ty.ptr Ty.i8

genStmt :: MonadGen m => [Stmt CoVar] -> m ()
genStmt (Foreign var c_type text:xs) = do
  let args = replicate (arity c_type) anything

  op <- extern (Llvm.mkName (T.unpack text)) args anything

  local (Vm.insert var op) $
    genStmt xs

genStmt (StmtLet (One (var, _, shallowEraseTys -> body)):xs) =
  case body of
    Atom (Lit lit) -> do
      let v_tag = var ^. covarId
          internal = Llvm.UnName (fromIntegral v_tag)

      name <- escape var
      init <- genLiteral lit

      -- Generate an LLVM constant of the "real" literal type..
      ~(Llvm.ConstantOperand the_global) <- global internal (litLlvmTy lit) init

      -- then massage the pointer to be of the real type.
      op <- global name anything (Const.BitCast the_global anything)

      local (Vm.insert var op) $
        genStmt xs

    Atom (Ref v _) -> do
      op <- view (at v . non (error ("llvmGen: unbound variable " ++ show v)))
      local (Vm.insert var op) $
        genStmt xs

    _ -> error "TODO: let of not-atom"

genStmt (StmtLet _binding:_xs) = undefined
genStmt (Type _type_name _constructors:_xs) = undefined

genStmt (RawCode asm:xs) = do
  let str = T.encodeUtf8 asm
  emitDefn (Llvm.ModuleInlineAssembly str)
  genStmt xs

genStmt [] = pure ()

genLiteral :: MonadGen m => Literal -> m Llvm.Constant
genLiteral (Int i)   = pure $ Llvm.Int 64 i
genLiteral (Float d) = pure $ Llvm.Float (Float.Double d)
genLiteral LitTrue   = pure $ Llvm.Int 1 0
genLiteral LitFalse  = pure $ Llvm.Int 1 1
genLiteral (Str t)   =
  let bytes = Bs.unpack (T.encodeUtf8 t)
      array = Llvm.Array { Llvm.memberType   = Ty.i8
                         , Llvm.memberValues = map (Llvm.Int 8 . fromIntegral) bytes
                         }
   in pure $
        Llvm.Struct { Llvm.isPacked     = True
                    , Llvm.memberValues = [ Llvm.Int 32 (fromIntegral (T.length t))
                                          , array ]
                    , Llvm.structName   = Nothing
                    }
genLiteral Unit   = pure $ Const.BitCast (Llvm.Int 8 0)   anything
genLiteral RecNil = pure $ Const.BitCast (Llvm.Int 8 255) anything

litLlvmTy :: Literal -> Llvm.Type
litLlvmTy Int{}    = Ty.i64
litLlvmTy Float{}  = Ty.double
litLlvmTy Str{}    = Ty.ptr $
  Ty.StructureType { Ty.isPacked = True
                   , Ty.elementTypes = [ Ty.i32, Ty.ptr Ty.i8 ]
                   }
litLlvmTy LitTrue{}  = Ty.i1
litLlvmTy LitFalse{} = Ty.i1
litLlvmTy Unit{}     = anything
litLlvmTy RecNil{}   = anything

shallowEraseTys :: Term v -> Term v
shallowEraseTys (Lam TypeArgument{} v) = v
shallowEraseTys (TyApp a _) = Atom a
shallowEraseTys (Cast a _ _) = Atom a
shallowEraseTys x = x
