{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, FlexibleContexts, TypeFamilies #-}

{-| Lowers "Syntax"'s nested patterns into "Core"'s flattened patterns.

  This (hopefully) uses the algorithm as described in "Compiling Pattern
  Matching to good Decision Trees"[1]. The heuristics used within this
  implementation are taken from that paper, "When Do Match-Compilation
  Heuristics Matter?"[2] and "Tree Pattern Matching for ML"[3].

  [1]: https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
  [2]: https://www.cs.tufts.edu/~nr/pubs/match.pdf
  [3]: https://pdfs.semanticscholar.org/1cd6/f28306e62341c11511422752f9a427fcc0e4.pdf
-}
module Core.Lower.Pattern
  ( lowerMatch
  , lowerMatch'
  , freshFromPat
  ) where

import Control.Monad.Reader
import Control.Monad.Namey

import qualified Data.HashMap.Strict as HMap
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.HashSet as HSet
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Bifunctor
import Data.Foldable
import Data.Triple
import Data.Maybe

import qualified Core.Core as C
import Core.Optimise (substituteInType, substituteInTys, fresh, freshFrom)
import Core.Types (unify, approximateAtomType, approximateType)
import Core.Lower.Basic
import Core.Builtin
import Core.Core
import Core.Var

import qualified Syntax as S
import Syntax.Var (Typed)

type ArmId = Int
type ArmSet = HSet.HashSet ArmId
type ArmMap = HMap.HashMap ArmId

data VariableSubst
  = VS
  { varFrom :: CoVar
  , varTo   :: CoVar
  , varTy   :: Type CoVar }
  deriving (Show)

-- | A leaf in a pattern matching tree.
data ArmLeaf
  = ArmLeaf
    { leafArm      :: ArmId
    , leafVarBinds :: [VariableSubst]
    , leafTyBinds  :: [VariableSubst] }
  deriving (Show)

-- | A node in the pattern matching tree, which checks against one or
-- more guarded expressions, else continues matching.
data ArmNode
  = ArmComplete
    { nodeArms    :: ArmSet
    , nodeSuccess :: [ArmLeaf] }
  | ArmMatch
    { nodeArms    :: ArmSet
    , nodeSuccess :: [ArmLeaf]
    , nodeAtom    :: Atom CoVar
    , nodeNodes   :: [(Pattern CoVar, ArmNode)] }
  deriving (Show)

data PatternRow
  = PR
    { rowArm      :: ArmId
    , rowPatterns :: VarMap.Map (S.Pattern Typed)
    , rowGuarded  :: Bool
    , rowVarBinds :: [VariableSubst]
    , rowTyBinds  :: [VariableSubst] }
  deriving (Show)

data ArmExpr m
  = ExprOnce (Term CoVar) [(CoVar, Type CoVar)] [(CoVar, Type CoVar)]
  | ExprLambda ([VariableSubst] -> [VariableSubst] -> m (Term CoVar))

lowerMatch :: MonadLower m => Atom CoVar -> [(S.Pattern Typed, Maybe (Term CoVar), Term CoVar)] -> m (Term CoVar)
lowerMatch (Ref r ty) cases = lowerMatch' r ty cases
lowerMatch a cases = do
  let ty = approximateAtomType a
  v <- fresh ValueVar
  Let (One (v, ty, Atom a)) <$> lowerMatch' v ty cases

lowerMatch' :: forall m. MonadLower m
            => CoVar -> Type CoVar -> [(S.Pattern Typed, Maybe (Term CoVar), Term CoVar)] -> m (Term CoVar)
lowerMatch' var ty cases = do
  let bodies :: ArmMap (ArmExpr m) = foldr (<>) mempty (zipWith (flip HMap.singleton . makeBody) cases [0..])
      guards :: ArmMap (ArmExpr m) = foldr (<>) mempty (zipWith makeGuard cases [0..])
  arms <- lowerOne (VarMap.singleton var ty) $
    zipWith (\(pat, g, _) arm ->
                PR { rowArm      = arm
                   , rowPatterns = VarMap.singleton var (normalisePattern pat)
                   , rowGuarded  = isJust g
                   , rowVarBinds = [], rowTyBinds = [] })
      cases [0..]
  flattenNode bodies guards arms

  where
    makeBody  (p, _, t) = ExprOnce t (patternVars' p) []
    makeGuard (_, Nothing, _) _ = mempty
    makeGuard (p, Just g, _)  k = HMap.singleton k (ExprOnce g (patternVars' p) [])

flattenLeaf :: forall m. MonadLower m
             => ArmMap (ArmExpr m) -> ArmMap (ArmExpr m)
             -> ArmLeaf -> Term CoVar
             -> m (Term CoVar)
flattenLeaf bodies guards (ArmLeaf n vs ts) rest = do
  body <- generate (bodies HMap.! n)
  case HMap.lookup n guards of
    Nothing -> pure body
    Just guard -> do
      guard' <- generate guard
      v <- fresh ValueVar
      pure . Let (One (v, tyBool, guard')) . Match (Ref v tyBool) $
        [ Arm (PatLit LitTrue) tyBool body [] []
        , Arm PatWildcard tyBool rest [] [] ]

  where
    generate (ExprLambda f) = f vs ts
    generate (ExprOnce bod _ _) =
      pure
      . (substituteInTys . VarMap.fromList . map (\(VS a b _) -> (a, VarTy b)) $ ts)
      $ foldr (\(VS f t ty) -> C.Let (One (t, ty, Atom (Ref f ty)))) bod vs

flattenNode :: forall m. MonadLower m
            => ArmMap (ArmExpr m) -> ArmMap (ArmExpr m)
            -> ArmNode -> m (Term CoVar)
flattenNode bodies guards (ArmComplete _ leafs)
  = foldrM (flattenLeaf bodies guards) undefined leafs
flattenNode bodies guards a@(ArmMatch _ leafs atom' children) = do
  let shared = HMap.keys . HMap.filter (>1) . countShared $ a
  (bodies', binds) <- foldrM generateBinds (bodies, id) shared
  (guards', binds) <- foldrM generateBinds (guards, binds) shared

  let child (pat, node) = do
        body <- flattenNode bodies' guards' node
        pure Arm { _armPtrn = pat
                 , _armTy = approximateAtomType atom'
                 , _armBody = body
                 , _armVars = patternVars pat
                 , _armTyvars = [] } -- TODO: Cry a lot

  match <- C.Match atom' <$> traverse child children
  binds <$> foldrM (flattenLeaf bodies' guards') match leafs

  where
    -- | Count the number of times each 'ArmId' occurs
    countShared :: ArmNode -> ArmMap Int
    countShared arm =
      let branches = HSet.foldr add mempty (nodeArms arm)
      in foldr (add . leafArm) branches (nodeSuccess arm)
      where add k = HMap.insertWith (+) k (1 :: Int)

    -- | Lift a pattern match into a lambda, passing arguments as values.
    generateBinds :: ArmId
                  -> (ArmMap (ArmExpr m), Term CoVar -> Term CoVar)
                  -> m (ArmMap (ArmExpr m), Term CoVar -> Term CoVar)
    generateBinds n (bods, build) =
      case HMap.lookup n bodies of
        Nothing -> pure (bods, build)
        Just ExprLambda{} -> pure (bods, build)
        Just (ExprOnce Atom{} _ _) -> pure (bods, build)
        Just (ExprOnce bod fv tvs) -> do
          v <- fresh ValueVar
          a <- fresh ValueVar

          let buildLam aty bod' = foldr (Lam . uncurry TypeArgument) (Lam (TermArgument a aty) bod') tvs
              buildLamTy fty = foldr (uncurry (ForallTy . Relevant)) fty tvs

              buildApp :: [VariableSubst] -> CoVar -> Type CoVar -> Atom CoVar -> m (Term CoVar)
              buildApp = buildApp' tvs
              buildApp' [] _ f fty arg = pure (App (Ref f fty) arg)
              buildApp' ((tv,_):tvs) ts f fty@(ForallTy (Relevant _) _ ftyr) arg = do
                f' <- fresh ValueVar
                let Just tv' = varFrom <$> find ((==tv) . varTo) ts
                    fty' = substituteInType (VarMap.singleton tv (VarTy tv')) ftyr
                Let (One (f', fty', TyApp (Ref f fty) (VarTy tv'))) <$> buildApp' tvs ts f' fty' arg
              buildApp' _ _ _ _ _ = error "Tyvar without a place to apply it"

          case fv of
            [] ->
              let ty = buildLamTy . ForallTy Irrelevant tyUnit . fromJust . approximateType $ bod
              in pure ( HMap.insert n (ExprLambda (\_ ts -> buildApp ts v ty (Lit Unit))) bods
                      , Let (One (v, ty, buildLam tyUnit bod)) . build )
            [(a', aty)] ->
              let ty = buildLamTy . ForallTy Irrelevant aty . fromJust . approximateType $ bod
              in pure ( HMap.insert n (ExprLambda (\_ ts -> buildApp ts v ty (Ref a' ty))) bods
                      , Let (One (v, ty, buildLam aty bod)) . build )
            _ -> do
              let aty = ValuesTy (map snd fv)
                  ty = buildLamTy . ForallTy Irrelevant aty . fromJust . approximateType $ bod

                  bod' = Match (Ref a aty)
                    [ Arm { _armPtrn = PatValues (map (uncurry Capture) fv)
                          , _armTy = aty
                          , _armBody = bod
                          , _armVars = fv
                          , _armTyvars = [] } ]

                  genApp vs ts = do
                    tvar <- fresh ValueVar
                    let tup = Values . flip map fv $ \(v, _) ->
                          let Just (VS v' _ ty') = find ((==v) . varTo) vs
                          in Ref v' ty'
                    Let (One (tvar, aty, tup)) <$> buildApp ts v ty (Ref tvar aty)

              pure ( HMap.insert n (ExprLambda genApp) bods
                   , Let (One (v, ty, buildLam aty bod')) . build )

-- | Find a "good" variable to match against, and match against it using
-- 'lowerOneOf'.
lowerOne :: MonadLower m
         => VarMap.Map (Type CoVar) -> [PatternRow]
         -> m ArmNode
lowerOne _ [] = error "Cannot have an empty match"
lowerOne tys rss = do
  state <- ask

  let v = findBest (heuristic state) Nothing vars
      Just ty = VarMap.lookup v tys
  if complete
  then pure ArmComplete
       { nodeArms    = foldr (HSet.insert . leafArm) mempty rMatching
       , nodeSuccess = rMatching }
  else lowerOneOf rMatching v ty tys rs

  where
    (rMatching, rs, complete) = trimMatching rss

    -- | Split any patterns into those which match unconditionally and
    -- any remaining patterns.
    trimMatching (PR arm pats guarded vBind tyBind:rs)
      | Just vBind' <- VarMap.foldrWithKey addTrivial (Just vBind) pats
      = first3 (ArmLeaf arm vBind' tyBind:) (if guarded then trimMatching rs else ([], [], True))
    trimMatching rs = ([], rs, False)

    addTrivial v (S.Capture v' (_, ty)) o = (VS v (mkVal v') (lowerType ty):) <$> o
    addTrivial _ S.Wildcard{} o = o
    addTrivial _ _ _ = Nothing

    vars = VarSet.toList (foldr (flip (VarMap.foldrWithKey (const . VarSet.insert)) . rowPatterns) mempty rs)

    findBest _ Nothing [] = error "Cannot have empty match"
    findBest h Nothing (v:vs) = findBest h (Just (v, h v)) vs
    findBest _ (Just (v, _)) [] = v
    findBest h (Just (v', s')) (v:vs) =
      let s = h v
          best = if s > s' then (v, s) else (v', s')
      in findBest h (Just best) vs

    -- | The general heuristic for the pattern matcher.
    --
    -- This tries several heuristics in order, using the next one in the
    -- event of a draw.
    heuristic :: LowerState -> CoVar -> (Int, Int, Int)
    heuristic state v = (neededPrefix v, branchingFactor state v, arity v)

    -- | Compute the needed prefix heuristic for a given row variable.
    --
    -- This scores a column based on how many of the leading rows requires it.
    neededPrefix :: CoVar -> Int
    neededPrefix var = go 0 rs where
      go n (r:rs)
        | Just p <- VarMap.lookup var (rowPatterns r)
        , not (isTrivialPat p)
        = go (n + 1) rs
      go n _ = n

    -- | Compute the "small branching factor" heuristic for a given row
    -- variable.
    --
    -- This scores a column based on the number of distinct constructors
    -- within it, favouring those with less distinct cases.
    branchingFactor :: LowerState -> CoVar -> Int
    branchingFactor state var =
      let hs = foldr (HSet.insert . maybe PatWildcard partialLower . VarMap.lookup var . rowPatterns) mempty rs
      in -HSet.size hs - (if partialComplete hs then 0 else 1) where

      partialLower S.Wildcard{} = PatWildcard
      partialLower S.Capture{} = PatWildcard
      partialLower (S.PLiteral l _) = PatLit (lowerLiteral l)
      partialLower (S.PSkolem p _ _) = partialLower p
      partialLower (S.Destructure v _ _) = Constr (mkCon v)
      partialLower (S.PRecord _ _) = PatRecord []
      partialLower (S.PAs p _ _) = partialLower p
      partialLower p = error ("Unhandled pattern " ++ show p)

      partialComplete hs = foldr ((||) . go) False hs where
        -- Somewhat strange, but every pattern will contain this, meaning
        -- every pattern is "complete".
        go PatWildcard{} = False
        -- Trivial patterns, always match
        go PatRecord{} = True
        go PatValues{} = True
        go (PatLit Unit) = True
        go (PatLit RecNil) = True
        -- Unbounded literals are never complete
        go (PatLit Int{}) = False
        go (PatLit Str{}) = False
        go (PatLit Float{}) = False
        -- Booleans are complete if 'True' and 'False' appears
        go (PatLit LitTrue) = HSet.member (PatLit LitFalse) hs
        go (PatLit LitFalse) = HSet.member (PatLit LitTrue) hs
        -- Ensure all constructors are present
        go (Constr v) = allCtors hs v
        go (Destr v _) = allCtors hs v

      allCtors hs = maybe False (VarSet.foldr ((&&) . flip HSet.member hs . Constr) True)  . getCtors

      getCtors :: CoVar -> Maybe VarSet.Set
      getCtors v = do
        ctor <- VarMap.lookup v (ctors state)
        ty <- getType ctor
        VarMap.lookup ty (types state)

      getType (ForallTy _ _ t) = getType t
      getType (ConTy a) = pure a
      getType (AppTy f _) = getType f
      getType _ = Nothing

    -- | Compute the "arity" heuristic for a given row variable.
    --
    -- This computes the total arity of the head constructor in each row
    -- (1 for destructures, n for records, 0 for everything else). We
    -- favour columns with lower arities.
    arity :: CoVar -> Int
    arity var = -foldr ((+) . go) 0 rs where
      go = maybe 0 arityOf . VarMap.lookup var . rowPatterns

      arityOf (S.PSkolem p _ _) = arityOf p
      arityOf (S.Destructure _ Nothing _) = 0
      arityOf (S.Destructure _ Just{} _) = 1
      arityOf (S.PRecord f _) = length f
      arityOf (S.PAs p _ _) = arityOf p
      arityOf _ = 0

-- | Lower a series of pattern rows, branching on the provided variable
-- and associated type.
--
-- This effectively "flattens" one column in the pattern matrix. For instance:
--
-- > v1 | v2
-- > a  | { x = b, y = c }
--
-- is reduced to
--
-- > v1 | v3 | v4
-- > a  | b  | c
--
-- And a match arm on @v2@. Note we make assume that the pattern rows are
-- non-empty and the leading row is non-trivial: that case is checked in
-- 'lowerOne'.
lowerOneOf :: forall m. MonadLower m
           => [ArmLeaf]
           -> CoVar -> Type CoVar
           -> VarMap.Map (Type CoVar) -> [PatternRow]
           -> m ArmNode
lowerOneOf preLeafs var ty tys = go [] . map prepare
  where
    prepare (PR arms pats gd vBind tyBind) =
      let pat = fromMaybe (S.Wildcard undefined) (VarMap.lookup var pats)
          (pat', vs) = unwrapAs pat
      in ( pat', PR arms (VarMap.delete var pats) gd (vs ++ vBind) tyBind )

    unwrapAs (S.PAs p v _) = (VS var (mkVal v) ty:) <$> unwrapAs p
    unwrapAs p = (p, [])

    go unc [] = lowerOne tys (reverse unc)
    go unc rs@((S.PRecord{},_):_) = goRows unc mempty rs
    go unc rs@((S.Destructure{},_):_) = goCtors unc mempty rs
    go unc rs@((S.PLiteral{},_):_) = goLiterals unc mempty rs
    go unc ((p, r):rs) = go (goGeneric p r:unc) rs

    -- | The fallback handler for "generic" fields
    goGeneric (S.Wildcard _) r = r
    goGeneric (S.Capture v _) (PR arm ps gd vBind tyBind)
      = PR arm ps gd (VS var (mkVal v) ty:vBind) tyBind
    goGeneric p _ = error ("Unhandled pattern " ++ show p)

    -- | Build up a mapping of (literals -> rows)
    goLiterals :: [PatternRow] -> Map.Map S.Lit [PatternRow]
               -> [(S.Pattern Typed, PatternRow)]
               -> m ArmNode
    goLiterals unc lits [] =
      let arms = map (\(l, ps) -> (PatLit (lowerLiteral l), [], reverse ps)) (Map.toList lits)
                 ++ [(PatWildcard, [], reverse unc)]
      in build arms
    goLiterals unc lits ((S.PLiteral l _,r):rs) =
      let lits' = fromMaybe unc (Map.lookup l lits)
      in goLiterals unc (Map.insert l (r:lits') lits) rs
    goLiterals unc lits ((p, r):rs) =
      let r' = goGeneric p r
          lits' = Map.map (r':) lits
      in goLiterals (r':unc) lits' rs

    -- | Build up a mapping of (field -> variable)s and extract the child
    -- patterns.
    goRows :: [PatternRow] -> Map.Map T.Text (CoVar, Type CoVar)
           -> [(S.Pattern Typed, PatternRow)]
           -> m ArmNode
    goRows unc fields [] = do
      let fields' = map (second (uncurry Capture)) (Map.toList fields)
      build [(PatRecord fields', map snd fields', reverse unc)]
    goRows unc fields ((S.PRecord fs _,PR arm ps gd vBind tyBind):rs) = do
      (fields', ps') <- foldrM flattenField (fields, ps) fs
      goRows (PR arm ps' gd vBind tyBind:unc) fields' rs
    goRows unc fields ((p, r):rs) = goRows (goGeneric p r:unc) fields rs

    -- | Flatten a field into the pattern map, using the existing mapping
    -- variable or adding a new one if required.
    flattenField (f, p) (fs, ps) = case Map.lookup f fs of
      Just (v, _) -> pure (fs, VarMap.insert v p ps)
      Nothing -> do
        v <- freshFromPat p
        pure ( Map.insert f (v, lowerType (S.getType p)) fs
             , VarMap.insert v p ps )

    -- | Build up a mapping of (constructors -> (contents variable, rows)).
    goCtors :: [PatternRow] -> VarMap.Map (Maybe (Capture CoVar), [PatternRow])
            -> [(S.Pattern Typed, PatternRow)]
            -> m ArmNode
    goCtors unc cases [] =
      let arms = map buildCtor (VarMap.toList cases) ++ [(PatWildcard, [], reverse unc)]
          buildCtor (c, (Nothing, pats)) = (Constr c, [], reverse pats)
          buildCtor (c, (Just cap, pats)) = (Destr c cap, [cap], reverse pats)
      in build arms
    goCtors unc cases ((S.Destructure v Nothing _,r):rs) =
      -- TODO: Work out whether we need to bind type variables at all.
      let v' = mkCon v
          (c, cases') = fromMaybe (Nothing, unc) (VarMap.lookup v' cases)
      in goCtors unc (VarMap.insert v' (c,r:cases') cases) rs
    goCtors unc cases (( S.Destructure v (Just p) (_, cty)
                       , PR arm pats gd vBind tyBind ):rs) = do
      let v' = mkCon v
      ~(Just cap@(Capture c _), cases') <- case VarMap.lookup v' cases of
        Nothing -> do
          ~(ForallTy Irrelevant x r) <- inst . fromJust <$> asks (VarMap.lookup (mkType v) . ctors)
          let Just s = r `unify` lowerType cty
              ty' = substituteInType s x
          (,unc) . Just . flip Capture ty' <$> freshFromPat p
        Just x -> pure x
      -- TODO: Work out whether we need to bind type variables at all.
      let r' = PR arm (VarMap.insert c p pats) gd vBind tyBind
      goCtors unc (VarMap.insert v' (Just cap, r':cases') cases) rs
    goCtors unc cases ((p, r):rs) =
      let r' = goGeneric p r
          cases' = VarMap.map (second (r':)) cases
      in goCtors (r':unc) cases' rs

    build pats = do
      nodes <- traverse (\(p, caps, r) ->
        (p,) <$> lowerOne (foldr (\(Capture v ty) -> VarMap.insert v ty) tys caps) r) pats
      let atom = Ref var ty
          allArms = foldr ((<>) . nodeArms . snd) mempty nodes
      pure (ArmMatch allArms preLeafs atom nodes)

    inst (ForallTy (Relevant _) _ t) = inst t
    inst t = t

-- | Normalise patterns, reducing a couple of the more complex cases to
-- their standard varieties.
normalisePattern :: S.Pattern Typed -> S.Pattern Typed
-- The "core" pattern cases, which need not be modified
normalisePattern p@S.Wildcard{} = p
normalisePattern p@S.Capture{} = p
normalisePattern p@S.PLiteral{} = p
normalisePattern p@(S.Destructure _ Nothing _) = p
normalisePattern (S.PSkolem p _ _) = normalisePattern p -- TODO: This!
normalisePattern (S.Destructure v (Just p) a) = S.Destructure v (Just (normalisePattern p)) a
normalisePattern (S.PAs p v a) = S.PAs (normalisePattern p) v a
normalisePattern (S.PRecord fs a) = S.PRecord (map (second normalisePattern) fs) a
-- Reduce these cases to something else
normalisePattern (S.PWrapper _ p _) = normalisePattern p
normalisePattern (S.PType p _ _) = normalisePattern p
normalisePattern S.PList{} = error "PList is handled by desugar"
normalisePattern (S.PTuple ps (pos, _)) = convert ps where
  convert [] = error "Empty tuple"
  convert [x] = normalisePattern x
  convert (x:xs) =
    let xs' = convert xs
    in S.PRecord [ ("_1", normalisePattern x), ("_2", xs') ]
                 (pos, S.TyExactRows [("_1", S.getType x), ("_2", S.getType xs')])

-- | Make a fresh variable from a pattern. This attempts to reuse names
-- if at all possible.
freshFromPat :: MonadNamey m => S.Pattern Typed -> m CoVar
freshFromPat (S.Capture v _) = freshFrom (mkVal v)
freshFromPat (S.PAs _ v _) = freshFrom (mkVal v)
freshFromPat _ = fresh ValueVar

-- | If this is a "trivial" pattern (it does not require a branch in
-- order to match).
isTrivialPat :: S.Pattern Typed -> Bool
isTrivialPat S.Wildcard{} = True
isTrivialPat S.Capture{} = True
isTrivialPat (S.PAs p _ _) = isTrivialPat p
isTrivialPat _ = False

patternVars :: Pattern CoVar -> [(CoVar, Type CoVar)]
patternVars (Destr _ p) = [captureVars p]
patternVars (PatRecord ps) = map (captureVars . snd) ps
patternVars (PatValues ps) = map captureVars ps
patternVars Constr{} = []
patternVars PatLit{} = []
patternVars PatWildcard{} = []

captureVars :: Capture CoVar -> (CoVar, Type CoVar)
captureVars (Capture v ty) = (v, ty)

patternVars' :: S.Pattern Typed -> [(CoVar, Type CoVar)]
patternVars' S.Wildcard{} = []
patternVars' S.PLiteral{} = []
patternVars' (S.Capture v (_, ty)) = [(mkVal v, lowerType ty)]
patternVars' (S.Destructure _ p _) = maybe [] patternVars' p
patternVars' (S.PAs p v (_, ty)) = (mkVal v, lowerType ty):patternVars' p
patternVars' (S.PRecord fs _) = concatMap (patternVars' . snd) fs
patternVars' (S.PWrapper _ p _) = patternVars' p
patternVars' (S.PSkolem p _ _) = patternVars' p
patternVars' (S.PType p _ _) = patternVars' p
patternVars' (S.PTuple ps _) = concatMap patternVars' ps
patternVars' S.PList{} = error "PList is handled by desugar"
