{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}

module Core.Optimise.Reduce
  ( Scope(..)
  , reducePass
  , reduceTerm
  , reduceAtom
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.VarMap as VarMap
import qualified Data.Text as T
import Data.Triple
import Data.List

import Control.Lens hiding (cons)
import Control.Arrow

import Core.Builtin
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

lookupBaseVar :: forall a. IsVar a => Scope a -> a -> Maybe (Term a)
lookupBaseVar s v = lookup v v where
  lookup u v = case (var u, var v) of
                 (Just (Atom (Ref u' _)), Just (Atom (Ref v' _)))
                   | Just e@(Atom (Ref v'' _)) <- var v' ->
                     if u' == v''
                     then Just (smallest v'' e)
                     else lookup u' v''
                 (_, t) -> t

  smallest v e = case var v of
                   Just e'@(Atom (Ref v' _)) | v' < v -> e'
                   _ -> e

  var :: a -> Maybe (Term a)
  var = flip VarMap.lookup (vars s) . toVar

lookupRawVar :: IsVar a => Scope a -> a -> a
lookupRawVar s v = case lookupBaseVar s v of
                  Just (TyApp (Ref v' _) _) -> lookupRawVar s v'
                  _ -> v

lookupRawTerm :: IsVar a => Scope a -> a -> Maybe (Term a)
lookupRawTerm s v = VarMap.lookup (toVar (lookupRawVar s v)) (vars s)

extendVar :: IsVar a => (a, Type a, Term a) -> Scope a -> Scope a
extendVar (v, _, e) s = s { vars = VarMap.insert (toVar v) e (vars s) }

extendVars :: IsVar a => [(a, Type a, Term a)] -> Scope a -> Scope a
extendVars vs s = foldr extendVar s vs

transformOver :: IsVar a => Scope a -> Term a -> Term a
transformOver = transT where
  mapA _ t@Ref{} = t
  mapA _ t@Lit{} = t

  transA s = reduceAtom s . mapA s

  mapT s (Atom a) = Atom (transA s a)
  mapT s (App f a) = App (transA s f) (transA s a)
  mapT s (Lam v b) = Lam v (transT s b)
  mapT s (TyApp f t) = TyApp (transA s f) t
  mapT s (Cast f t) = Cast (transA s f) t
  mapT s (Extend t rs) = Extend (transA s t) (map (third3 (transA s)) rs)
  mapT s (Values xs) = Values (map (transA s) xs)
  mapT s (Let (One var) body) =
    let var' = third3 (transT s) var
        body' = transT (extendVar var' s) body
     in Let (One var') body'
  mapT s (Let (Many vars) body) =
    let vars' = map (third3 (transT (extendVars vars s))) vars
        body' = transT (extendVars vars' s) body
     in Let (Many vars') body'
  mapT s (Match test branches) =
    let test' = transA s test
        branches' = map (armBody %~ transT s) branches
    in Match test' branches'

  transT s = reduceTerm s . mapT s

reducePass :: IsVar a => [Stmt a] -> [Stmt a]
reducePass = reduceStmts (Scope mempty mempty mempty) where
  reduceStmts _ [] = []
  reduceStmts s (x@Foreign{}:xs) = x:reduceStmts s xs
  reduceStmts s (StmtLet (One var):xs) =
    let var' = third3 (transformOver s) var
        xs' = reduceStmts (extendVar var' s) xs
     in StmtLet (One var'):xs'
  reduceStmts s (StmtLet (Many vars):xs) =
    let vars' = map (third3 (transformOver (extendVars vars s))) vars
        xs' = reduceStmts (extendVars vars' s) xs
     in StmtLet (Many vars'):xs'
  reduceStmts s (x@(Type v cases):xs) =
    let s' = s { types = VarMap.insert (toVar v) cases (types s)
               , cons = VarMap.union (VarMap.fromList (map (first toVar) cases)) (cons s) }
    in x:reduceStmts s' xs

reduceAtom :: IsVar a => Scope a -> Atom a -> Atom a

-- Beta reduction (let case)
reduceAtom s a@(Ref v _) = {-# SCC "Reduce.beta_let" #-}
  case lookupBaseVar s v of
    Just (Atom d) | trivialAtom d -> d
    _ -> a

reduceAtom _ e = e

reduceTerm :: forall a. IsVar a => Scope a -> Term a -> Term a

-- Empty expressions
reduceTerm _ (Extend e []) = {-# SCC "Reduce.empty_extend" #-} Atom e

-- Let of bottom conversion
reduceTerm _ (Let (One (v, _, TyApp (Ref var errort) _)) (Let (One (_, _, App (Ref v' _) msg)) cont))
  | v == v'
  , isError var = {-# SCC "Reduce.let_bottom" #-}
    let Just ty = approximateType cont
        errTy = ForallTy Irrelevant tyString ty

        newerr = TyApp (Ref var errort) ty
        errapp = App (Ref v errTy) msg
     in Let (One (v, errTy, newerr)) errapp

-- Let Commuting conversion
reduceTerm s (Let (One (x, xt, Let (One (y, yt, yval)) xval)) rest) =
  {-# SCC "Reduce.commute_let" #-}
  reduceTerm s $ Let (One (y, yt, yval)) $ reduceTerm s (Let (One (x, xt, xval)) rest)

-- Match commuting conversion (trivial case)
reduceTerm _ (Let (One (v, vt, Match t [a])) r) =
  {-# SCC "Reduce.commute_match" #-}
  Match t [a & armBody %~ appendBody v vt r]

-- Trivial matches
reduceTerm s (Match t (Arm { _armPtrn = Capture v _, _armTy = ty, _armBody = body }:_)) = {-# SCC "Reduce.trivial_match" #-}
  reduceTerm s $ Let (One (v, ty, Atom t)) body
reduceTerm s (Match t bs) = {-# SCC "Reduce.fold_cases" #-}
  case foldCases bs of
    Left  Arm { _armBody = b } -> b
    Right bs' ->
      case last bs' of
        -- If we were really smart, we could strip _all_ cases which are shadowed by
        -- another. For now, simply detect the case `error @a "Nope"`
        Arm { _armPtrn = Capture _ _
            , _armBody = Let (One (tyApp, _, TyApp (Ref err _) _))
                        (App (Ref tyApp' _) (Lit _)) }
          | isError err
          , toVar tyApp == toVar tyApp'
          , isComplete s (init bs ^.. each . armPtrn)
          -> Match t (init bs')
        _ -> Match t bs'

  where foldCases [] = Right []
        foldCases (a@Arm { _armPtrn = p, _armBody = body }:xs) = case reducePattern s t p of
          PatternFail -> foldCases xs
          PatternUnknown subst -> Right (substArm a subst body:either pure id (foldCases xs))
          PatternPartial subst -> Right [substArm a subst body]
          PatternComplete subst -> Left (substArm a subst body)

        substArm :: Arm a -> Subst a -> Term a -> Arm a
        substArm a@Arm{_armTyvars = []} subst body
          -- In the trivial case we can do a plain old substitution
          = a & armBody .~ substitute subst body
        substArm a@Arm{_armTyvars = ts, _armVars = vs} subst body
          -- Otherwise we look up types and attempt to unify them.
          = let Just tySubst = VarMap.foldrWithKey (foldVar vs . fromVar) (Just mempty) (subst :: VarMap.Map (Atom a))
            in a { _armVars = map (second (substituteInType tySubst)) vs
                 -- Substitute tyvars and remove those which have been remapped
                 , _armTyvars = map (second (substituteInType tySubst)) $ filter (not . flip VarMap.member tySubst . toVar . fst) ts
                 -- Substitute atoms and tyvars
                 , _armBody = substituteInTys tySubst $ substitute subst body
                 }

        foldVar :: [(a, Type a)] -> a -> Atom a -> Maybe (VarMap.Map (Type a)) -> Maybe (VarMap.Map (Type a))
        foldVar _ _ _ Nothing = Nothing
        foldVar vs v a (Just sol) = do
          vty <- snd <$> find ((==v) . fst) vs
          aty <- approximateAtomType a
          unifyWith sol vty aty

-- Beta reduction (function case)
-- reduceTerm s (App (Lam (TermArgument var ty) body) ex) = {-# SCC "Reduce.beta_function" #-}
--   reduceTerm s $ Let (One (var, ty, Atom ex)) body
-- reduceTerm s (TyApp (Lam (TypeArgument var _) body) tp) = {-# SCC "Reduce.beta_type_function" #-}
--   reduceTerm s (substituteInTys (VarMap.singleton (toVar var) tp) body)

-- Eta conversion (function case)
reduceTerm _ (Lam (TermArgument var _) (App r (Ref var' _)))
  | var == var' = {-# SCC "Reduce.eta_term" #-} Atom r
reduceTerm _ (Lam (TypeArgument var _) (TyApp r (VarTy var')))
  | var == var' = {-# SCC "Reduce.eta_type" #-} Atom r

-- Eta reduction (let case)
reduceTerm _ (Let (One (v, _, term)) (Atom (Ref v' _)))
  | v == v' = {-# SCC "Reduce.eta_let" #-} term

reduceTerm _ (Let (One (_, ty, term)) (Atom (Lit Unit)))
  | ty == tyUnit = {-# SCC "Reduce.eta_let" #-} term

-- Eta reduction (let case) for single constructor types which
-- take no arguments
reduceTerm s (Let (One (_, ty, term)) (Atom (Ref _ ty')))
  -- We need the types to be equivalent and to have a single, no-args constructor
  | ty `unifyClosed` ty'
  , Just tyName <- unwrapTy ty
  , Just [(_, unwrapTy -> Just _)] <- VarMap.lookup (toVar tyName) (types s)
  = {-# SCC "Reduce.eta_let" #-} term
  where
    unwrapTy (ForallTy Relevant{} _ t) = unwrapTy t
    unwrapTy (ConTy v) = Just v
    unwrapTy _ = Nothing

-- Coercion reduction
reduceTerm s (Cast (Ref v _) c)
  | Just (Cast a c') <- lookupVar s v
  , Just (l, r) <- relates c
  , Just (l', r') <- relates c'
  , unifyClosed r l'
  , unifyClosed l r'
  = Atom a

reduceTerm _ (Cast a co)
  | redundantCo co = Atom a
  | otherwise = Cast a (squishCoercion co)

-- Constant fold
reduceTerm s e@(App (Ref v _) (Ref a _))
  | Just (Values xs) <- lookupVar s a
  , n <- toVar v
  = case xs of
      -- Primitive integer reductions
      [Lit (Int l), Lit (Int r)] | n == vOpAdd -> num (l + r)
      [Lit (Int l), Lit (Int r)] | n == vOpSub -> num (l - r)
      [Lit (Int l), Lit (Int r)] | n == vOpMul -> num (l * r)
      [Lit (Int l), Lit (Int r)] | n == vOpDiv -> num (l `div` r)
      [Lit (Int l), Lit (Int r)] | n == vOpExp -> num (l ^ r)
      [Lit (Int l), Lit (Int r)] | n == vOpLt -> bool (l < r)
      [Lit (Int l), Lit (Int r)] | n == vOpGt -> bool (l > r)
      [Lit (Int l), Lit (Int r)] | n == vOpLe -> bool (l <= r)
      [Lit (Int l), Lit (Int r)] | n == vOpGe -> bool (l >= r)

      -- Partial integer reductions
      [x, Lit (Int 0)] | n == vOpAdd -> Atom x
      [Lit (Int 0), x] | n == vOpAdd -> Atom x
      [Lit (Int 0), x] | n == vOpSub -> Atom x
      [x, Lit (Int 1)] | n == vOpMul -> Atom x
      [Lit (Int 1), x] | n == vOpMul -> Atom x
      [_, Lit (Int 0)] | n == vOpMul -> num 0
      [Lit (Int 0), _] | n == vOpMul -> num 0
      [x, Lit (Int 1)] | n == vOpDiv -> Atom x

      -- Primitive string reductions
      [Lit (Str l), Lit (Str r)]  | n == vOpConcat -> str (l `T.append` r)

      -- Partial string reductions
      [x, Lit (Str "")] | n == vOpConcat -> Atom x
      [Lit (Str ""), x] | n == vOpConcat -> Atom x

      _ -> e
  where num = Atom . Lit . Int
        str = Atom . Lit . Str
        bool x = Atom (Lit (if x then LitTrue else LitFalse))

reduceTerm _ e = e

type Subst a = VarMap.Map (Atom a)

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

extract :: PatternResult a -> Subst a
extract PatternFail = mempty
extract (PatternUnknown s) = s
extract (PatternPartial s) = s
extract (PatternComplete s) = s

reducePattern :: forall a. IsVar a => Scope a -> Atom a -> Pattern a -> PatternResult a

-- A capture always yield a complete pattern
reducePattern _ term (Capture a _) = PatternComplete (VarMap.singleton (toVar a) term)

-- Literals are relatively easy to accept/reject
reducePattern _ _ (PatLit RecNil) = PatternComplete mempty
reducePattern _ _ (PatLit Unit) = PatternComplete mempty
reducePattern _ (Lit l') (PatLit l)
  | l == l'   = PatternComplete mempty
  | otherwise = PatternFail

-- If we're matching against a known constructor it is easy to accept or reject
reducePattern s (Ref v _) (Constr c)
  | lookupRawVar s v == c = PatternComplete mempty

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
  allMatching (PatValues xs) = all allMatching xs
  allMatching _ = False

reducePattern _ _ _ = PatternUnknown mempty

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

  -- For unboxed tuples we simply determine if the first pattern is complete, or
  -- if some later one could be. This is definitely not a complete check, but it
  -- is correct.
  isComplete' (PatValues ps:xs) = all (isComplete' . pure) ps || isComplete' xs

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
  unwrapTy (ForallTy _ _ t) = unwrapTy t
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

redundantCo :: IsVar a => Coercion a -> Bool
redundantCo c
  | Just (a, b) <- relates c = a == b
  | otherwise = False

appendBody :: a -> Type a -> Term a -> Term a -> Term a
appendBody v ty r (Let bind b) = Let bind (appendBody v ty r b)
appendBody v ty r b = Let (One (v, ty, b)) r

trivialAtom :: Atom a -> Bool
trivialAtom Ref{} = True
trivialAtom (Lit (Str t)) = T.length t <= 8
trivialAtom (Lit _) = True
