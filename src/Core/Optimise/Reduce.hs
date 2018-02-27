{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Reduce
  ( reduceTerm
  , reduceAtom
  , reduceTermPass
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.Semigroup
import Data.Triple
import Data.List

import Control.Monad.Reader

import Syntax (Resolved, Var(..))

import Core.Optimise hiding (CoAtom, CoTerm, CoPattern, CoType, find)
import qualified Core.Optimise as Co

type CoAtom = Co.CoAtom (Var Resolved)
type CoTerm = Co.CoTerm (Var Resolved)
type CoPattern = Co.CoPattern (Var Resolved)
type CoType = Co.CoType (Var Resolved)

reduceTermPass :: TransformPass
reduceTermPass = pass (\e -> asks (`reduceTerm` e))

reduceAtom :: TransState -> CoAtom -> CoAtom

-- Eta conversion
reduceAtom _ (CoaLam Small (var, _) (CotApp r (CoaRef var' _))) | var == var' = r
reduceAtom _ (CoaLam Small (var, _) (CotApp r (CoaRef var' _))) | var == var' = r

reduceAtom _ e = e

reduceTerm :: TransState -> CoTerm -> CoTerm

-- Empty expressions
reduceTerm _ (CotLet [] e) = e
reduceTerm _ (CotExtend e []) = CotAtom e

-- Commuting conversion
reduceTerm s (CotLet [(x, xt, CotLet [(y, yt, yval)] xval)] rest) | x `VarSet.notMember`freeIn yval
  = reduceTerm s $ CotLet [(y, yt, yval)] $ CotLet [(x, xt, xval)] rest

-- Trivial matches
reduceTerm s (CotMatch t ((CopCapture v _, ty, body):_)) = reduceTerm s $ CotLet [(v, ty, CotAtom t)] body
reduceTerm s (CotMatch t bs) =
  case foldCases bs of
    Left  (_, _, b) -> b
    Right bs' -> CotMatch t bs'

  where foldCases [] = Right []
        foldCases ((p, ty, body):xs) = case reducePattern s t p of
                             PatternFail -> foldCases xs
                             PatternUnknown subst -> Right ((p, ty, substitute subst body) : either pure id (foldCases xs))
                             PatternPartial subst -> Right [(p, ty, substitute subst body)]
                             PatternComplete subst -> Left (p, ty, substitute subst body)

-- Beta reduction
reduceTerm s (CotApp (CoaLam Small (var, ty) body) ex) = reduceTerm s $ CotLet [(var, ty, CotAtom ex)] body
reduceTerm s (CotTyApp (CoaLam Big (var, _) body) tp) = reduceTerm s (substituteInTys (Map.singleton var tp) body)

-- Inline variable references
reduceTerm s e@(CotAtom(CoaRef v _)) =
  case Map.lookup v (vars s) of
    Just d@(CotAtom CoaRef{}) -> reduceTerm s d
    Just d@(CotAtom CoaLit{}) -> d
    _ -> e

-- Constant fold
reduceTerm s e@(CotApp (CoaRef f1 _) (CoaLit r1)) =
  case Map.lookup f1 (vars s) of
    Just (CotApp (CoaRef (TgInternal v) _) (CoaLit l1)) ->
      case (v, l1, r1) of
        ("+",  ColInt l, ColInt r) -> num (l + r)
        ("-",  ColInt l, ColInt r) -> num (l - r)
        ("*",  ColInt l, ColInt r) -> num (l * r)
        ("/",  ColInt l, ColInt r) -> num (l `div` r)
        ("**", ColInt l, ColInt r) -> num (l ^ r)
        ("<" , ColInt l, ColInt r) -> bool (l < r)
        (">",  ColInt l, ColInt r) -> bool (l > r)
        (">=", ColInt l, ColInt r) -> bool (l >= r)
        ("<=", ColInt l, ColInt r) -> bool (l <= r)

        ("&&", ColTrue, ColTrue)   -> bool True
        ("&&", _, _)               -> bool False
        ("||", ColFalse, ColFalse) -> bool False
        ("||", _, _)               -> bool True

        ("^", ColStr l, ColStr r)  -> str (l `T.append` r)

        _ -> e
    _ -> e
  where num = CotAtom . CoaLit . ColInt
        str = CotAtom . CoaLit . ColStr
        bool x = CotAtom (CoaLit (if x then ColTrue else ColFalse))

reduceTerm _ e = e

type Subst = Map.Map (Var Resolved) CoAtom

data PatternResult
  = PatternFail
  | PatternUnknown Subst
  | PatternPartial Subst
  | PatternComplete Subst
  deriving (Show)

instance Semigroup PatternResult where
  PatternFail <> _ = PatternFail
  _ <> PatternFail = PatternFail

  (PatternUnknown s) <> t = PatternUnknown (s <> extract t)
  s <> (PatternUnknown t) = PatternUnknown (extract s <> t)

  (PatternPartial s) <> t = PatternPartial (s <> extract t)
  s <> (PatternPartial t) = PatternPartial (extract s <> t)

  (PatternComplete s) <> (PatternComplete t) = PatternComplete (s <> t)

extract :: PatternResult -> Subst
extract PatternFail = mempty
extract (PatternUnknown s) = s
extract (PatternPartial s) = s
extract (PatternComplete s) = s

simplifyVar :: TransState -> Var Resolved -> Var Resolved
simplifyVar s v = case Map.lookup v (vars s) of
                  Just (CotTyApp (CoaRef v' _) _) -> simplifyVar s v'
                  Just (CotAtom (CoaRef v' _)) -> simplifyVar s v'
                  _ -> v

simplifyTerm :: TransState -> Var Resolved -> Maybe CoTerm
simplifyTerm s v = Map.lookup (simplifyVar s v) (vars s)

simplifyRecord :: TransState -> CoAtom -> [(T.Text, CoType, CoAtom)]
simplifyRecord s (CoaRef v _) = case Map.lookup v (vars s) of
                                  Just (CotAtom r) -> simplifyRecord s r
                                  Just (CotExtend r fs) -> fs ++ simplifyRecord s r
                                  _ -> []
simplifyRecord _ _ = []

reducePattern :: TransState -> CoAtom -> CoPattern -> PatternResult

-- A capture always yield a complete pattern
reducePattern _ term (CopCapture a _) = PatternComplete (Map.singleton a term)

-- Literals are relatively easy to accept/reject
reducePattern _ _ (CopLit ColRecNil) = PatternComplete Map.empty
reducePattern _ (CoaLit l') (CopLit l) | l == l'   = PatternComplete Map.empty
                                       | otherwise = PatternFail

-- If we're matching against a known constructor it is easy to accept or reject
-- TODO: Reject applications/flat vars in the other method
reducePattern s (CoaRef v _) (CopConstr c) | simplifyVar s v == c = PatternComplete Map.empty

                                           | isCon' s (simplifyVar s v) = PatternFail
                                           | Just (CotApp (CoaRef c' _) _) <- simplifyTerm s v
                                           , isCon' s (simplifyVar s c') = PatternFail

reducePattern s (CoaRef v _) (CopDestr c a) | Just (CotApp (CoaRef c' _) a') <- simplifyTerm s v
                                            , simplifyVar s c' == c = reducePattern s a' a

                                            | isCon' s (simplifyVar s v) = PatternFail
                                            | Just (CotApp (CoaRef c' _) _) <- simplifyTerm s v
                                            , isCon' s (simplifyVar s c') = PatternFail

-- Attempt to reduce the field
reducePattern s e (CopExtend rest fs) = foldr ((<>) . handle) (reducePattern s e rest) fs where
  handle (f, p) = case find ((==f) . fst3) fs' of
                    Nothing -> if allMatching p
                               then PatternPartial mempty
                               else PatternUnknown mempty
                    Just (_, _, e) -> reducePattern s e p

  fs' = simplifyRecord s e

reducePattern _ _ _ = PatternUnknown Map.empty

allMatching :: CoPattern -> Bool
allMatching (CopLit ColRecNil) = True
allMatching (CopCapture _ _) = True
allMatching (CopExtend r fs) = allMatching r && all (allMatching . snd) fs
allMatching _ = False
