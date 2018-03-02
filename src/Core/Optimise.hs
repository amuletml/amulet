module Core.Optimise
  ( substitute, substituteInTys
  , module Core.Core
  , Var(..)
  , fresh
  ) where

import qualified Data.Map.Strict as Map

import Data.VarSet (IsVar(..))

import Control.Monad.Infer (fresh)

import Control.Arrow (second)
import Data.Triple

import Core.Core
import Syntax

substitute :: IsVar a => Map.Map a (CoAtom a) -> CoTerm a -> CoTerm a
substitute m = term where
  term (CotAtom a) = CotAtom (atom a)
  term (CotApp f x) = CotApp (atom f) (atom x)
  term (CotLet vs x) = CotLet (map (third3 term) vs) (term x)
  term (CotMatch x vs) = CotMatch (atom x) (map (third3 term) vs)
  term (CotExtend e rs) = CotExtend (atom e) (map (third3 atom) rs)
  term (CotTyApp f t) = CotTyApp (atom f) t

  atom x@(CoaRef v _) = Map.findWithDefault x v m
  atom (CoaLam s v b) = CoaLam s v (term b)
  atom x@CoaLit{} = x

substituteInTys :: IsVar a => Map.Map a (CoType a) -> CoTerm a -> CoTerm a
substituteInTys m = term where
  term (CotAtom a) = CotAtom (atom a)
  term (CotApp f x) = CotApp (atom f) (atom x)
  term (CotLet vs x) = CotLet (map (\(v, t, e) -> (v, gotype t, term e)) vs) (term x)
  term (CotMatch x vs) = CotMatch (atom x) (map (\(v, t, e) -> (v, gotype t, term e)) vs)
  term (CotExtend e rs) = CotExtend (atom e) (map (third3 atom) rs)
  term (CotTyApp f t) = CotTyApp (atom f) (gotype t)

  atom (CoaRef v t) = CoaRef v (gotype t)
  atom (CoaLam s (v, t) b) = CoaLam s (v, gotype t) (term b)
  atom x@CoaLit{} = x

  gotype x@(CotyVar v) = Map.findWithDefault x v m
  gotype x@CotyCon{} = x
  gotype (CotyForall v t) = CotyForall v (gotype t)
  gotype (CotyArr a b) = CotyArr (gotype a) (gotype b)
  gotype (CotyApp f x) = CotyApp (gotype f) (gotype x)
  gotype (CotyRows v rs) = CotyRows (gotype v) (map (second gotype) rs)
  gotype (CotyExactRows rs) = CotyExactRows (map (second gotype) rs)
  gotype CotyStar = CotyStar
