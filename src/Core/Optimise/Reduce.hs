{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Reduce
  ( Scope(..)
  , reducePass
  , reduceTerm
  , reduceAtom
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import qualified Data.VarMap as VarMap
import qualified Data.Text as T
import Data.VarSet (toVar,IsVar)
import Data.Semigroup
import Data.Triple
import Data.List

import Control.Arrow

import Core.Optimise

data Scope a = Scope { vars :: VarMap.Map (CoTerm a)
                     , types :: VarMap.Map [(a, CoType a)]
                     , cons :: VarMap.Map (CoType a) }
  deriving (Show)

isCon :: IsVar a => Scope a -> a -> Bool
isCon s var = VarMap.member (toVar var) (cons s)

lookupVar :: IsVar a => Scope a -> a -> Maybe (CoTerm a)
lookupVar s v = VarMap.lookup (toVar v) (vars s)

lookupRawVar :: IsVar a => Scope a -> a -> a
lookupRawVar s v = case VarMap.lookup (toVar v) (vars s) of
                  Just (CotTyApp (CoaRef v' _) _) -> lookupRawVar s v'
                  Just (CotAtom (CoaRef v' _)) -> lookupRawVar s v'
                  _ -> v

lookupRawTerm :: IsVar a => Scope a -> a -> Maybe (CoTerm a)
lookupRawTerm s v = VarMap.lookup (toVar (lookupRawVar s v)) (vars s)

extendVars :: IsVar a => [(a, CoType a, CoTerm a)] -> Scope a -> Scope a
extendVars vs s = s { vars = foldr (\(v, _, e) m -> VarMap.insert (toVar v) e m) (vars s) vs }

transformOver :: IsVar a => Scope a -> CoTerm a -> CoTerm a
transformOver = transT where
  mapA _ t@CoaRef{} = t
  mapA _ t@CoaLit{} = t
  mapA s (CoaLam t v b) = CoaLam t v (transT s b)

  transA s = reduceAtom s . mapA s

  mapT s (CotAtom a) = CotAtom (transA s a)
  mapT s (CotApp f a) = CotApp (transA s f) (transA s a)
  mapT s (CotTyApp f t) = CotTyApp (transA s f) t
  mapT s (CotExtend t rs) = CotExtend (transA s t) (map (third3 (transA s)) rs)
  mapT s (CotLet vars body) =
    let vars' = map (third3 (transT (extendVars vars s))) vars
        body' = transT (extendVars vars' s) body
    in CotLet vars' body'
  mapT s (CotMatch test branches) =
    let test' = transA s test
        branches' = map (third3 (transT s)) branches
    in CotMatch test' branches'

  transT s = reduceTerm s . mapT s

reducePass :: IsVar a => [CoStmt a] -> [CoStmt a]
reducePass = reduceStmts (Scope mempty mempty mempty) where
  reduceStmts _ [] = []
  reduceStmts s (x@CosForeign{}:xs) = x:reduceStmts s xs
  reduceStmts s (CosLet vars:xs) =
    let vars' = map (third3 (transformOver (extendVars vars s))) vars
        xs' = reduceStmts (extendVars vars' s) xs
    in CosLet vars':xs'
  reduceStmts s (x@(CosType v cases):xs) =
    let s' = s { types = VarMap.insert (toVar v) cases (types s)
               , cons = VarMap.union (VarMap.fromList (map (first toVar) cases)) (cons s) }
    in x:reduceStmts s' xs

reduceAtom :: IsVar a => Scope a -> CoAtom a -> CoAtom a

-- Eta conversion (function case)
reduceAtom _ (CoaLam Small (var, _) (CotApp r (CoaRef var' _)))
  | var == var' = {-# SCC "Reduce.eta_term" #-} r
reduceAtom _ (CoaLam Big (var, _) (CotTyApp r (CotyVar var')))
  | var == var' = {-# SCC "Reduce.eta_type" #-} r

-- Beta reduction (let case)
reduceAtom s a@(CoaRef v _) = {-# SCC "Reduce.beta_let" #-}
  case lookupVar s v of
    Just (CotAtom d@CoaRef{}) -> reduceAtom s d
    Just (CotAtom d@CoaLit{}) -> d
    _ -> a

reduceAtom _ e = e

reduceTerm :: IsVar a => Scope a -> CoTerm a -> CoTerm a

-- Empty expressions
reduceTerm _ (CotLet [] e) = {-# SCC "Reduce.empty_let" #-} e
reduceTerm _ (CotExtend e []) = {-# SCC "Reduce.empty_extend" #-} CotAtom e

-- Commuting conversion
reduceTerm s (CotLet [(x, xt, CotLet [(y, yt, yval)] xval)] rest)
  | not (occursInTerm x yval) = {-# SCC "Reduce.commute_let" #-}
    reduceTerm s $ CotLet [(y, yt, yval)] $ reduceTerm s (CotLet [(x, xt, xval)] rest)

-- Trivial matches
reduceTerm s (CotMatch t ((CopCapture v _, ty, body):_)) = {-# SCC "Reduce.trivial_match" #-}
  reduceTerm s $ CotLet [(v, ty, CotAtom t)] body
reduceTerm s (CotMatch t bs) = {-# SCC "Reduce.fold_cases" #-}
  case foldCases bs of
    Left  (_, _, b) -> b
    Right bs' -> CotMatch t bs'

  where foldCases [] = Right []
        foldCases ((p, ty, body):xs) = case reducePattern s t p of
          PatternFail -> foldCases xs
          PatternUnknown subst -> Right ((p, ty, substitute subst body) : either pure id (foldCases xs))
          PatternPartial subst -> Right [(p, ty, substitute subst body)]
          PatternComplete subst -> Left (p, ty, substitute subst body)

-- Beta reduction (function case)
reduceTerm s (CotApp (CoaLam Small (var, ty) body) ex) = {-# SCC "Reduce.beta_function" #-}
  reduceTerm s $ CotLet [(var, ty, CotAtom ex)] body
reduceTerm s (CotTyApp (CoaLam Big (var, _) body) tp) = {-# SCC "Reduce.beta_type_function" #-}
  reduceTerm s (substituteInTys (Map.singleton var tp) body)

-- Eta reduction (let case)
reduceTerm _ (CotLet [(v, _, term)] (CotAtom (CoaRef v' _)))
  | v == v' && not (occursInTerm v term) = {-# SCC "Reduce.eta_let" #-} term

-- Constant fold
reduceTerm s e@(CotApp (CoaRef f1 _) (CoaLit r1)) = {-# SCC "Reduce.constant_fold" #-}
  case lookupVar s f1 of
    Just (CotApp (CoaRef v _) (CoaLit l1)) | TgInternal n <- toVar v  ->
      case (n, l1, r1) of
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

type Subst a = Map.Map a (CoAtom a)

data PatternResult a
  = PatternFail
  | PatternUnknown (Subst a)
  | PatternPartial (Subst a)
  | PatternComplete (Subst a)
  deriving (Show)

instance Ord a => Semigroup (PatternResult a) where
  PatternFail <> _ = PatternFail
  _ <> PatternFail = PatternFail

  (PatternUnknown s) <> t = PatternUnknown (s <> extract t)
  s <> (PatternUnknown t) = PatternUnknown (extract s <> t)

  (PatternPartial s) <> t = PatternPartial (s <> extract t)
  s <> (PatternPartial t) = PatternPartial (extract s <> t)

  (PatternComplete s) <> (PatternComplete t) = PatternComplete (s <> t)

extract :: Ord a => PatternResult a -> Subst a
extract PatternFail = mempty
extract (PatternUnknown s) = s
extract (PatternPartial s) = s
extract (PatternComplete s) = s

reducePattern :: IsVar a => Scope a -> CoAtom a -> CoPattern a -> PatternResult a

-- A capture always yield a complete pattern
reducePattern _ term (CopCapture a _) = PatternComplete (Map.singleton a term)

-- Literals are relatively easy to accept/reject
reducePattern _ _ (CopLit ColRecNil) = PatternComplete Map.empty
reducePattern _ _ (CopLit ColUnit) = PatternComplete Map.empty
reducePattern _ (CoaLit l') (CopLit l)
  | l == l'   = PatternComplete Map.empty
  | otherwise = PatternFail

-- If we're matching against a known constructor it is easy to accept or reject
reducePattern s (CoaRef v _) (CopConstr c)
  | lookupRawVar s v == c = PatternComplete Map.empty

  | isCon s (lookupRawVar s v) = PatternFail
  | Just (CotApp (CoaRef c' _) _) <- lookupRawTerm s v
  , isCon s (lookupRawVar s c') = PatternFail

reducePattern s (CoaRef v _) (CopDestr c a)
  | Just (CotApp (CoaRef c' _) a') <- lookupRawTerm s v
  , lookupRawVar s c' == c = reducePattern s a' a

  | isCon s (lookupRawVar s v) = PatternFail
  | Just (CotApp (CoaRef c' _) _) <- lookupRawTerm s v
  , isCon s (lookupRawVar s c') = PatternFail

-- Attempt to reduce the field
reducePattern s e (CopExtend rest fs) = foldr ((<>) . handle) (reducePattern s e rest) fs where
  handle (f, p) =
    case find ((==f) . fst3) fs' of
      Nothing -> if allMatching p
                    then PatternPartial mempty
                    else PatternUnknown mempty
      Just (_, _, e) -> reducePattern s e p

  fs' = simplifyRecord e

  simplifyRecord (CoaRef v _) =
    case lookupVar s v of
      Just (CotAtom r) -> simplifyRecord r
      Just (CotExtend r fs) -> fs ++ simplifyRecord r
      _ -> []
  simplifyRecord _ = []

  allMatching (CopLit ColRecNil) = True
  allMatching (CopLit ColUnit) = True
  allMatching (CopCapture _ _) = True
  allMatching (CopExtend r fs) = allMatching r && all (allMatching . snd) fs
  allMatching _ = False

reducePattern _ _ _ = PatternUnknown Map.empty
