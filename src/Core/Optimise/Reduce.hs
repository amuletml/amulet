{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

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
import Core.Types

data Scope a = Scope { vars :: VarMap.Map (Term a)
                     , types :: VarMap.Map [(a, Type a)]
                     , cons :: VarMap.Map (Type a) }
  deriving (Show)

isCon :: IsVar a => Scope a -> a -> Bool
isCon s var = VarMap.member (toVar var) (cons s)

lookupVar :: IsVar a => Scope a -> a -> Maybe (Term a)
lookupVar s v = VarMap.lookup (toVar v) (vars s)

lookupRawVar :: IsVar a => Scope a -> a -> a
lookupRawVar s v = case VarMap.lookup (toVar v) (vars s) of
                  Just (TyApp (Ref v' _) _) -> lookupRawVar s v'
                  Just (Atom (Ref v' _)) -> lookupRawVar s v'
                  _ -> v

lookupRawTerm :: IsVar a => Scope a -> a -> Maybe (Term a)
lookupRawTerm s v = VarMap.lookup (toVar (lookupRawVar s v)) (vars s)

extendVars :: IsVar a => [(a, Type a, Term a)] -> Scope a -> Scope a
extendVars vs s = s { vars = foldr (\(v, _, e) m -> VarMap.insert (toVar v) e m) (vars s) vs }

transformOver :: IsVar a => Scope a -> Term a -> Term a
transformOver = transT where
  mapA _ t@Ref{} = t
  mapA _ t@Lit{} = t
  mapA s (Lam t v b) = Lam t v (transT s b)

  transA s = reduceAtom s . mapA s

  mapT s (Atom a) = Atom (transA s a)
  mapT s (App f a) = App (transA s f) (transA s a)
  mapT s (TyApp f t) = TyApp (transA s f) t
  mapT s (Cast f t) = Cast (transA s f) t
  mapT s (Extend t rs) = Extend (transA s t) (map (third3 (transA s)) rs)
  mapT s (Let vars body) =
    let vars' = map (third3 (transT (extendVars vars s))) vars
        body' = transT (extendVars vars' s) body
    in Let vars' body'
  mapT s (Match test branches) =
    let test' = transA s test
        branches' = map (third3 (transT s)) branches
    in Match test' branches'

  transT s = reduceTerm s . mapT s

reducePass :: IsVar a => [Stmt a] -> [Stmt a]
reducePass = reduceStmts (Scope mempty mempty mempty) where
  reduceStmts _ [] = []
  reduceStmts s (x@Foreign{}:xs) = x:reduceStmts s xs
  reduceStmts s (StmtLet vars:xs) =
    let vars' = map (third3 (transformOver (extendVars vars s))) vars
        xs' = reduceStmts (extendVars vars' s) xs
    in StmtLet vars':xs'
  reduceStmts s (x@(Type v cases):xs) =
    let s' = s { types = VarMap.insert (toVar v) cases (types s)
               , cons = VarMap.union (VarMap.fromList (map (first toVar) cases)) (cons s) }
    in x:reduceStmts s' xs

reduceAtom :: IsVar a => Scope a -> Atom a -> Atom a

-- Eta conversion (function case)
reduceAtom _ (Lam Small (var, _) (App r (Ref var' _)))
  | var == var' = {-# SCC "Reduce.eta_term" #-} r
reduceAtom _ (Lam Big (var, _) (TyApp r (VarTy var')))
  | var == var' = {-# SCC "Reduce.eta_type" #-} r

-- Beta reduction (let case)
reduceAtom s a@(Ref v _) = {-# SCC "Reduce.beta_let" #-}
  case lookupVar s v of
    Just (Atom d@Ref{}) -> reduceAtom s d
    Just (Atom d@Lit{}) -> d
    _ -> a

reduceAtom _ e = e

reduceTerm :: IsVar a => Scope a -> Term a -> Term a

-- Empty expressions
reduceTerm _ (Let [] e) = {-# SCC "Reduce.empty_let" #-} e
reduceTerm _ (Extend e []) = {-# SCC "Reduce.empty_extend" #-} Atom e

-- Commuting conversion
reduceTerm s (Let [(x, xt, Let [(y, yt, yval)] xval)] rest)
  | not (occursInTerm x yval) = {-# SCC "Reduce.commute_let" #-}
    reduceTerm s $ Let [(y, yt, yval)] $ reduceTerm s (Let [(x, xt, xval)] rest)

-- Trivial matches
reduceTerm s (Match t ((Capture v _, ty, body):_)) = {-# SCC "Reduce.trivial_match" #-}
  reduceTerm s $ Let [(v, ty, Atom t)] body
reduceTerm s (Match t bs) = {-# SCC "Reduce.fold_cases" #-}
  case foldCases bs of
    Left  (_, _, b) -> b
    Right bs' ->
      case last bs' of
        -- If we were really smart, we could strip _all_ cases which are shadowed by
        -- another. For now, simply detect the case `error @a "Nope"`
        (Capture _ _, _, Let [(tyApp, _, TyApp (Ref err _) _)]
                            (App (Ref tyApp' _) (Lit _)))
          | TgInternal "error" <- toVar err
          , toVar tyApp == toVar tyApp'
          , isComplete s (map fst3 (init bs'))
          -> Match t (init bs')
        _ -> Match t bs'

  where foldCases [] = Right []
        foldCases ((p, ty, body):xs) = case reducePattern s t p of
          PatternFail -> foldCases xs
          PatternUnknown subst -> Right ((p, ty, substitute subst body) : either pure id (foldCases xs))
          PatternPartial subst -> Right [(p, ty, substitute subst body)]
          PatternComplete subst -> Left (p, ty, substitute subst body)

-- Beta reduction (function case)
reduceTerm s (App (Lam Small (var, ty) body) ex) = {-# SCC "Reduce.beta_function" #-}
  reduceTerm s $ Let [(var, ty, Atom ex)] body
reduceTerm s (TyApp (Lam Big (var, _) body) tp) = {-# SCC "Reduce.beta_type_function" #-}
  reduceTerm s (substituteInTys (Map.singleton var tp) body)

-- Eta reduction (let case)
reduceTerm _ (Let [(v, _, term)] (Atom (Ref v' _)))
  | v == v' && not (occursInTerm v term) = {-# SCC "Reduce.eta_let" #-} term

-- Coercion reduction
reduceTerm s (Cast (Ref v _) c)
  | Just (Cast a c') <- lookupVar s v
  , Just (l, r) <- relates c
  , Just (l', r') <- relates c'
  , Just uni <- unify r l'
  , Just _ <- unifyWith uni l r'
  = Atom a

-- Constant fold
reduceTerm s e@(App (Ref f1 _) (Lit r1)) = {-# SCC "Reduce.constant_fold" #-}
  case lookupVar s f1 of
    Just (App (Ref v _) (Lit l1)) | TgInternal n <- toVar v  ->
      case (n, l1, r1) of
        ("+",  Int l, Int r) -> num (l + r)
        ("-",  Int l, Int r) -> num (l - r)
        ("*",  Int l, Int r) -> num (l * r)
        ("/",  Int l, Int r) -> num (l `div` r)
        ("**", Int l, Int r) -> num (l ^ r)
        ("<" , Int l, Int r) -> bool (l < r)
        (">",  Int l, Int r) -> bool (l > r)
        (">=", Int l, Int r) -> bool (l >= r)
        ("<=", Int l, Int r) -> bool (l <= r)

        ("&&", LitTrue, LitTrue)   -> bool True
        ("&&", _, _)               -> bool False
        ("||", LitFalse, LitFalse) -> bool False
        ("||", _, _)               -> bool True

        ("^", Str l, Str r)  -> str (l `T.append` r)

        _ -> e
    _ -> e
  where num = Atom . Lit . Int
        str = Atom . Lit . Str
        bool x = Atom (Lit (if x then LitTrue else LitFalse))

reduceTerm _ e = e

type Subst a = Map.Map a (Atom a)

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

reducePattern :: forall a. IsVar a => Scope a -> Atom a -> Pattern a -> PatternResult a

-- A capture always yield a complete pattern
reducePattern _ term (Capture a _) = PatternComplete (Map.singleton a term)

-- Literals are relatively easy to accept/reject
reducePattern _ _ (PatLit RecNil) = PatternComplete Map.empty
reducePattern _ _ (PatLit Unit) = PatternComplete Map.empty
reducePattern _ (Lit l') (PatLit l)
  | l == l'   = PatternComplete Map.empty
  | otherwise = PatternFail

-- If we're matching against a known constructor it is easy to accept or reject
reducePattern s (Ref v _) (Constr c)
  | lookupRawVar s v == c = PatternComplete Map.empty

  | isCon s (lookupRawVar s v) = PatternFail
  | Just (App (Ref c' _) _) <- lookupRawTerm s v
  , isCon s (lookupRawVar s c') = PatternFail

reducePattern s (Ref v _) (Destr c a)
  | Just (App (Ref c' _) a') <- lookupRawTerm s v
  , lookupRawVar s c' == c = reducePattern s a' a

  | isCon s (lookupRawVar s v) = PatternFail
  | Just (App (Ref c' _) _) <- lookupRawTerm s v
  , isCon s (lookupRawVar s c') = PatternFail

-- Attempt to reduce the field
reducePattern s e (PatExtend rest fs) = foldr ((<>) . handle) (reducePattern s e rest) fs where
  handle :: (T.Text, Pattern a) -> PatternResult a
  handle (f, p) =
    case find ((==f) . fst3) fs' of
      Nothing -> if allMatching p
                    then PatternPartial mempty
                    else PatternUnknown mempty
      Just (_, _, e) -> reducePattern s e p

  fs' = simplifyRecord e

  simplifyRecord (Ref v _) =
    case lookupVar s v of
      Just (Atom r) -> simplifyRecord r
      Just (Extend r fs) -> fs ++ simplifyRecord r
      _ -> []
  simplifyRecord _ = []

  allMatching (PatLit RecNil) = True
  allMatching (PatLit Unit) = True
  allMatching (Capture _ _) = True
  allMatching (PatExtend r fs) = allMatching r && all (allMatching . snd) fs
  allMatching _ = False

reducePattern _ _ _ = PatternUnknown Map.empty

isComplete :: IsVar a => Scope a -> [Pattern a] -> Bool
isComplete s = isComplete' where
  isComplete' :: IsVar a => [Pattern a] -> Bool
  isComplete' [] = False
  -- Trivial always-true captures
  isComplete' (Capture{}:_) = True
  isComplete' (PatLit Unit:_) = True
  isComplete' (PatLit RecNil:_) = True

  isComplete' (PatLit (Int _):xs) = isComplete' xs
  isComplete' (PatLit (Str _):xs) = isComplete' xs
  isComplete' (PatLit (Float _):xs) = isComplete' xs
  isComplete' (PatLit LitTrue:xs)    = hasBool LitTrue  xs
  isComplete' (PatLit LitFalse:xs)   = hasBool LitFalse xs

  -- Trivial, always-true captures
  isComplete' xs@(Destr v _:_)    = hasSumTy (buildSumTy v) xs
  isComplete' xs@(Constr v:_)     = hasSumTy (buildSumTy v) xs

  -- Skip record types for now
  isComplete' xs@(PatExtend{}:_) = hasRecord Map.empty xs

  hasBool _ [] = False
  hasBool _ (Capture _ _:_) = True
  hasBool l (PatLit l':_) | l /= l' = True
  hasBool p (_:xs) = hasBool p xs

  buildSumTy v =
    let Just ty = VarMap.lookup (toVar v) (cons s)
        Just cases = VarMap.lookup (toVar (unwrapTy ty)) (types s)
    in foldr (\(a, _) m -> Map.insert (toVar a) (Just []) m) Map.empty cases

  -- Oh goodness, this is horrible. This attempts to extract
  -- the type name from a constructor's type
  unwrapTy (ForallTy _ t) = unwrapTy t
  unwrapTy (ArrTy _ t) = unwrapTy t
  unwrapTy (AppTy t _) = unwrapTy t
  unwrapTy (ConTy v) = v
  unwrapTy ty = error (show ty)

  hasSumTy m [] = Map.foldr (\ps r -> r && maybe True isComplete' ps) True m
  hasSumTy _ (Capture{}:_) = True
  hasSumTy m (Constr v:xs) = hasSumTy (Map.insert (toVar v) Nothing m) xs
  hasSumTy m (Destr v b:xs) = hasSumTy (Map.adjust (\(Just ps) -> Just (b:ps)) (toVar v) m) xs
  hasSumTy m (_:xs) = hasSumTy m xs

  hasRecord :: IsVar a => Map.Map T.Text [Pattern a] -> [Pattern a] -> Bool
  hasRecord m [] = Map.foldr (\ps r -> r && isComplete' ps) True m
  hasRecord _ (Capture{}:_) = True
  hasRecord m (e@PatExtend{}:xs) =
    let m' = foldr (\(f, p) r -> Map.insertWith (++) f [p] r) m (flattenExtend e)
    in hasRecord m' xs
  hasRecord m (_:xs) = hasRecord m xs

  flattenExtend (PatExtend p fs) = flattenExtend p ++ fs
  flattenExtend _ = []
