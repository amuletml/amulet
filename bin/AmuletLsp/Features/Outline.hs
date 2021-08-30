{-# LANGUAGE OverloadedStrings, DuplicateRecordFields, FlexibleContexts, TypeFamilies #-}
module AmuletLsp.Features.Outline (getOutline) where

import qualified Data.Text as T
import Data.Foldable
import Data.Spanned
import Data.Span

import Language.LSP.Types
import AmuletLsp.Features

import Syntax

import Text.Pretty.Semantic hiding (line)

getOutline :: [Toplevel Parsed] -> [DocumentSymbol]
getOutline = concatMap getTop where
  getTop :: Toplevel Parsed -> [DocumentSymbol]
  getTop (LetStmt _ _ bindings _) = concatMap getBinding bindings
  getTop t@(ForeignVal _ v _ _ _) = [ mk v SkFunction t Nothing Nothing ]

  getTop t@(TypeDecl _ v args ctors _) =
    [ mk v SkStruct t (getApp v args) (map getCtor <$> ctors) ]
  getTop t@(TySymDecl _ v args _ _) = [ mk v SkClass t (getApp v args) Nothing ]
  getTop t@(TypeFunDecl _ v args _ _ _) = [ mk v SkClass t (getApp v args) Nothing ]

  getTop t@(Module _ v (ModStruct ts _)) =
    [ mk v SkModule t Nothing (Just (concatMap getTop ts)) ]
  getTop t@(Module _ v _) = [ mk v SkModule t Nothing Nothing ]
  getTop Open{} = [] -- Skip open and include unconditionally for now.
  getTop Include{} = []

  getTop t@(Class v _ _ args _ ms _) =
    [ mk v SkClass t (getApp v args) (Just (concatMap getClassItem ms)) ]
  getTop Instance{} = []
  getTop DeriveInstance{} = []

  getBinding :: Binding Parsed -> [DocumentSymbol]
  getBinding (Binding v vp _ _ _) = [ mk v SkFunction vp Nothing Nothing ]
  getBinding b@(Matching p _ _) = getPattern (spanOf b) [] p
  getBinding TypedMatching{} = []

  getClassItem :: ClassItem Parsed -> [DocumentSymbol]
  getClassItem c@(MethodSig v _ _) = [ mk v SkMethod c Nothing Nothing ]
  getClassItem DefaultMethod{} = []
  getClassItem c@(AssocType v args _ _) = [ mk v SkClass c (getApp v args) Nothing ]

  -- For now, constructors don't provide any additional detail. Their definition
  -- /is/ their type signature, so not clear if it's worth exposing it or not.
  getCtor c@(UnitCon _ v _) = mk v SkConstructor c Nothing Nothing
  getCtor c@(ArgCon _ v _ _) = mk v SkConstructor c Nothing Nothing
  getCtor c@(GadtCon _ v _ _) = mk v SkConstructor c Nothing Nothing

  getPattern :: Span -> [DocumentSymbol] -> Pattern Parsed -> [DocumentSymbol]
  getPattern _   ds Wildcard{}           = ds
  getPattern def ds p@(Capture v _)      = mkWith v SkVariable def p Nothing Nothing:ds
  getPattern def ds (Destructure _ p _)  = foldl' (getPattern def) ds p
  getPattern def ds n@(PAs p v _)        = getPattern def (mkWith v SkVariable def n Nothing Nothing:ds) p
  getPattern def ds (PType p _ _)        = getPattern def ds p
  getPattern def ds (POr p q _)          = getPattern def ds p ++ getPattern def ds q
  getPattern def ds (PTuple ps _)        = foldl' (getPattern def) ds ps
  getPattern def ds (PRecord ps _)       = foldl' (\x -> getPattern def x . snd) ds ps
  getPattern def ds (PList ps _)         = foldl' (getPattern def) ds ps
  getPattern def ds (PGadtCon _ _ _ p _) = foldl' (getPattern def) ds p
  getPattern _   ds PLiteral{}           = ds

getName :: Var Parsed -> T.Text
getName (Name n) = n
getName InModule{} = "?"

-- | Render a type with a series of arguments.
getApp :: Var Parsed -> [TyConArg Parsed] -> Maybe T.Text
getApp v args = Just . renderBasic . hsep $ pretty (TyCon v undefined :: Type Parsed) : map pretty args

-- | Construct a document symbol from a name and node.
mk :: Spanned a
   => Var Parsed -> SymbolKind -> a
   -> Maybe T.Text -> Maybe [DocumentSymbol]
   -> DocumentSymbol
mk name kind node = mkWith name kind (spanOf node) node

-- | Construct a document symbol from a name, node and custom position.
mkWith :: Spanned a
   => Var Parsed -> SymbolKind -> Span -> a
   -> Maybe T.Text -> Maybe [DocumentSymbol]
   -> DocumentSymbol
mkWith name kind range node detail children =
  DocumentSymbol
  { _name = getName name
  , _detail = detail
  , _kind = kind
  , _deprecated = Nothing
  , _range = rangeOf range
  , _selectionRange = firstLine (rangeOf (spanOf node))
  , _children = List <$> children
  , _tags = Nothing
  }
