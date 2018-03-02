{-# LANGUAGE OverloadedStrings #-}

module Core.Optimise.Reduce
  ( reduceTermPass
  , reduceTerm
  , reduceAtom
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarSet as VarSet
import qualified Data.Text as T
import Data.VarSet (toVar,IsVar)
import Data.Semigroup
import Data.Triple
import Data.List

import Core.Optimise
import Core.Optimise.Transform

reduceTermPass :: IsVar a => [CoStmt a] -> [CoStmt a]
reduceTermPass = transformStmts reduceTerm reduceAtom mempty

reduceAtom :: IsVar a => Transform CoAtom a

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

reduceTerm :: IsVar a => Transform CoTerm a

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

simplifyRecord :: IsVar a => Scope a -> CoAtom a -> [(T.Text, CoType a, CoAtom a)]
simplifyRecord s (CoaRef v _) =
  case lookupVar s v of
    Just (CotAtom r) -> simplifyRecord s r
    Just (CotExtend r fs) -> fs ++ simplifyRecord s r
    _ -> []
simplifyRecord _ _ = []

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
  | simplifyVar s v == c = PatternComplete Map.empty

  | isCon s (simplifyVar s v) = PatternFail
  | Just (CotApp (CoaRef c' _) _) <- simplifyTerm s v
  , isCon s (simplifyVar s c') = PatternFail

reducePattern s (CoaRef v _) (CopDestr c a)
  | Just (CotApp (CoaRef c' _) a') <- simplifyTerm s v
  , simplifyVar s c' == c = reducePattern s a' a

  | isCon s (simplifyVar s v) = PatternFail
  | Just (CotApp (CoaRef c' _) _) <- simplifyTerm s v
  , isCon s (simplifyVar s c') = PatternFail

-- Attempt to reduce the field
reducePattern s e (CopExtend rest fs) = foldr ((<>) . handle) (reducePattern s e rest) fs where
  handle (f, p) =
    case find ((==f) . fst3) fs' of
      Nothing -> if allMatching p
                    then PatternPartial mempty
                    else PatternUnknown mempty
      Just (_, _, e) -> reducePattern s e p

  fs' = simplifyRecord s e

reducePattern _ _ _ = PatternUnknown Map.empty

allMatching :: CoPattern a -> Bool
allMatching (CopLit ColRecNil) = True
allMatching (CopLit ColUnit) = True
allMatching (CopCapture _ _) = True
allMatching (CopExtend r fs) = allMatching r && all (allMatching . snd) fs
allMatching _ = False
