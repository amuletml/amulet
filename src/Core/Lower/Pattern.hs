{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, FlexibleContexts #-}

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
  ) where

import Control.Monad.Reader
import Control.Monad.Namey
import Control.Arrow

import qualified Data.HashMap.Strict as HMap
import qualified Data.VarMap as VarMap
import qualified Data.VarSet as VarSet
import qualified Data.HashSet as HSet
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Foldable
import Data.Maybe

import qualified Core.Core as C
import Core.Types (unify, approximateAtomType, approximateType)
import Core.Optimise (substituteInType, substituteInTys, fresh, freshFrom)
import Core.Lower.Basic
import Core.Builtin
import Core.Core
import Core.Var

import qualified Syntax as S
import Syntax.Var (Var(..), Typed)
import Syntax.Pretty()

type ArmId = Int
type ArmSet = HSet.HashSet ArmId
type ArmMap = HMap.HashMap ArmId

data VariableSubst
  = VS
  { varFrom :: CoVar
  , varTo   :: CoVar
  , varTy   :: Type CoVar }
  deriving (Show)

-- | A node in the pattern matching tree
data ArmNode
  -- | A leaf in a pattern matching tree.
  = ArmLeaf
    { leafArm      :: ArmId
    , leafVarBinds :: [VariableSubst]
    , leafTyBinds  :: [VariableSubst] }
  -- | An internal node in a pattern matching tree, which
  -- matches against a pattern.
  | ArmMatch
    { matchArms    :: ArmSet
    , matchAtom    :: Atom CoVar
    , matchNodes   :: [(Pattern CoVar, ArmNode)] }
  deriving (Show)

data PatternRow
  = PR
    { rowArm      :: ArmId
    , rowPatterns :: VarMap.Map (S.Pattern Typed)
    , rowVarBinds :: [VariableSubst]
    , rowTyBinds  :: [VariableSubst] }
  deriving (Show)

data ArmBody m
  = BodyOnce (Term CoVar) [(CoVar, Type CoVar)] [(CoVar, Type CoVar)]
  | BodyLambda ([VariableSubst] -> [VariableSubst] -> m (Term CoVar))

freeInArm :: ArmNode -> ArmSet
freeInArm (ArmLeaf n _ _) = HSet.singleton n
freeInArm (ArmMatch s _ _) = s

lowerMatch :: MonadLower m => Atom CoVar -> [(S.Pattern Typed, Term CoVar)] -> m (Term CoVar)
lowerMatch (Ref r ty) cases = lowerMatch' r ty cases
lowerMatch a cases = do
  let ty = approximateAtomType a
  v <- fresh ValueVar
  Let (One (v, ty, Atom a)) <$> lowerMatch' v ty cases

lowerMatch' :: forall m. MonadLower m => CoVar -> Type CoVar -> [(S.Pattern Typed, Term CoVar)] -> m (Term CoVar)
lowerMatch' var ty cases = do
  let bodies :: ArmMap (ArmBody m) = foldr (<>) mempty (zipWith (flip HMap.singleton . makeBody) cases [0..])
  arms <- lowerOneOf var ty (VarMap.singleton var ty)
    (zipWith (\(pat, _) arm -> PR arm (VarMap.singleton var (normalisePattern pat)) [] []) cases [0..])
  flattenResult bodies arms

  where makeBody (p, t) = BodyOnce t (patternVars' p) []

flattenResult :: forall m. MonadLower m => ArmMap (ArmBody m) -> ArmNode -> m (Term CoVar)
flattenResult bodies (ArmLeaf n vs ts) =
  case bodies HMap.! n of
    BodyLambda f -> f vs ts
    BodyOnce bod _ _ ->
        pure
      . (substituteInTys . VarMap.fromList . map (\(VS a b _) -> (a, VarTy b)) $ ts)
      $ foldr (\(VS f t ty) -> C.Let (One (t, ty, Atom (Ref f ty)))) bod vs

flattenResult bodies (ArmMatch _ atom' children) = do
  let shared = HMap.keys . HMap.filter (>1) . foldr countShared mempty $ children
  (bodies', binds) <- foldrM generateBinds (bodies, id) shared

  let child (pat, node) = do
        body <- flattenResult bodies' node
        pure Arm { _armPtrn = pat
                 , _armTy = approximateAtomType atom'
                 , _armBody = body
                 , _armVars = patternVars pat
                 , _armTyvars = [] } -- TODO: Cry a lot
  binds . C.Match atom' <$> traverse child children

  where
    -- | Count the number of times each 'ArmId' occurs
    countShared :: (Pattern CoVar, ArmNode) -> ArmMap Int -> ArmMap Int
    countShared (_, arm) branches = HSet.foldr (flip (HMap.insertWith (+)) 1) branches (freeInArm arm)

    -- | Lift a pattern match into a lambda, passing arguments as values.
    generateBinds :: ArmId
                  -> (ArmMap (ArmBody m), Term CoVar -> Term CoVar)
                  -> m (ArmMap (ArmBody m), Term CoVar -> Term CoVar)
    generateBinds n (bods, build) =
      case bodies HMap.! n of
        BodyLambda{} -> pure (bods, build)
        BodyOnce Atom{} _ _ -> pure (bods, build)
        BodyOnce bod fv tvs -> do
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
              in pure ( HMap.insert n (BodyLambda (\_ ts -> buildApp ts v ty (Lit Unit))) bods
                      , Let (One (v, ty, buildLam tyUnit bod)) . build )
            [(a', aty)] ->
              let ty = buildLamTy . ForallTy Irrelevant aty . fromJust . approximateType $ bod
              in pure ( HMap.insert n (BodyLambda (\_ ts -> buildApp ts v ty (Ref a' ty))) bods
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

              pure ( HMap.insert n (BodyLambda genApp) bods
                   , Let (One (v, ty, buildLam aty bod')) . build )

-- | Find a "good" variable to match against, and match against it using
-- 'lowerOneOf'.
lowerOne :: MonadLower m
         => VarMap.Map (Type CoVar) -> [PatternRow]
         -> m ArmNode
lowerOne _ [] = error "Cannot have an empty match"
lowerOne _ (PR arm pats vBind tyBind:_)
  | Just vBind' <- VarMap.foldrWithKey addTrivial (Just vBind) pats
  = pure (ArmLeaf arm vBind' tyBind)
  where
    addTrivial v (S.Capture (TvName v') (_, ty)) o = (VS v (mkVal v') (lowerType ty):) <$> o
    addTrivial _ S.Wildcard{} o = o
    addTrivial _ _ _ = Nothing
lowerOne tys rs =
  let v = findBest heuristic Nothing vars
      Just ty = VarMap.lookup v tys
  in lowerOneOf v ty tys rs

  where
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
    heuristic :: CoVar -> (Int, Int, Int)
    heuristic v = (neededPrefix v, branchingFactor v, arity v)

    -- | Compute the needed prefix heuristic for a given row variable.
    --
    -- This scores a column based on how many of the leading rows requires it.
    neededPrefix :: CoVar -> Int
    neededPrefix var = go 0 rs where
      go n (PR _ ps _ _:rs)
        | Just p <- VarMap.lookup var ps
        , not (isTrivialPat p)
        = go (n + 1) rs
      go n _ = n

    -- | Compute the "small branching factor" heuristic for a given row
    -- variable.
    --
    -- This scores a column based on the number of distinct constructors
    -- within it, favouring those with less distinct cases.
    --
    -- __TODO:__ This is not entirely correctly implemented, as we should
    -- subtract one in the case where one or more constructors.
    branchingFactor :: CoVar -> Int
    branchingFactor var = -HSet.size (foldr go mempty rs) where
      go (PR _ ps _ _) s
        | Just p <- VarMap.lookup var ps
        = HSet.insert (partialLower p) s
      go _ s = s

      partialLower S.Wildcard{} = PatWildcard
      partialLower S.Capture{} = PatWildcard
      partialLower (S.PLiteral l _) = PatLit (lowerLiteral l)
      partialLower (S.PSkolem p _ _) = partialLower p
      partialLower (S.Destructure (TvName v) _ _) = Constr (mkCon v)
      partialLower (S.PRecord _ _) = PatExtend (Capture (CoVar 0 "?" ValueVar) (VarTy tyvarA)) []
      partialLower p = error ("Unhandled pattern " ++ show p)

    -- | Compute the "arity" heuristic for a given row variable.
    --
    -- This computes the total arity of the head constructor in each row
    -- (1 for destructures, n for records, 0 for everything else). We
    -- favour columns with lower arities.
    arity :: CoVar -> Int
    arity var = -foldr ((+) . go) 0 rs where
      go (PR _ ps _ _)
        | Just p <- VarMap.lookup var ps
        = arityOf p
      go _ = 0

      arityOf (S.PSkolem p _ _) = arityOf p
      arityOf (S.Destructure _ Nothing _) = 0
      arityOf (S.Destructure _ Just{} _) = 1
      arityOf (S.PRecord f _) = length f
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
           => CoVar -> Type CoVar
           -> VarMap.Map (Type CoVar) -> [PatternRow]
           -> m ArmNode
lowerOneOf var ty tys = go [] . map (\(PR arm pats vBind tyBind) ->
                                       ( fromMaybe (S.Wildcard undefined) (VarMap.lookup var pats)
                                       , PR arm (VarMap.delete var pats) vBind tyBind ))
  where
    go unc [] = lowerOne tys (reverse unc)
    go unc rs@((S.PRecord{},_):_) = goRows unc mempty rs
    go unc rs@((S.Destructure{},_):_) = goCtors unc mempty rs
    go unc rs@((S.PLiteral{},_):_) = goLiterals unc mempty rs
    go unc ((p, r):rs) = go (goGeneric p r:unc) rs

    -- | The fallback handler for "generic" fields
    goGeneric (S.Wildcard _) r = r
    goGeneric (S.Capture (TvName v) _) (PR arm ps vBind tyBind)
      = PR arm ps (VS var (mkVal v) ty:vBind) tyBind
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
          ty' = case ty of
                  RowsTy b rs ->
                    case filter (flip Map.notMember fields . fst) rs of
                      [] -> b
                      rs' -> RowsTy b rs'
                  _ -> NilTy
      v <- flip Capture ty' <$> fresh ValueVar
      build [(PatExtend v fields', v:map snd fields', reverse unc)]
    goRows unc fields ((S.PRecord fs _,PR arm ps vBind tyBind):rs) = do
      (fields', ps') <- foldrM flattenField (fields, ps) fs
      goRows (PR arm ps' vBind tyBind:unc) fields' rs
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
    goCtors :: [PatternRow] -> HMap.HashMap CoVar (Maybe (Capture CoVar), [PatternRow])
            -> [(S.Pattern Typed, PatternRow)]
            -> m ArmNode
    goCtors unc cases [] =
      let arms = map buildCtor (HMap.toList cases) ++ [(PatWildcard, [], reverse unc)]
          buildCtor (c, (Nothing, pats)) = (Constr c, [], reverse pats)
          buildCtor (c, (Just cap, pats)) = (Destr c cap, [cap], reverse pats)
      in build arms
    goCtors unc cases ((S.Destructure (TvName v) Nothing _,r):rs) =
      -- TODO: Work out whether we need to bind type variables at all.
      let v' = mkCon v
          (c, cases') = fromMaybe (Nothing, unc) (HMap.lookup v' cases)
      in goCtors unc (HMap.insert v' (c,r:cases') cases) rs
    goCtors unc cases (( S.Destructure (TvName v) (Just p) (_, cty)
                       , PR arm pats vBind tyBind ):rs) = do
      let v' = mkCon v
      (Just cap@(Capture c _), cases') <- case HMap.lookup v' cases of
        Nothing -> do
          ForallTy Irrelevant x r <- inst . fromJust <$> asks (Map.lookup v . ctors)
          let Just s = r `unify` lowerType cty
              ty' = substituteInType s x
          (,unc) . Just . flip Capture ty' <$> freshFromPat p
        Just x -> pure x
      -- TODO: Work out whether we need to bind type variables at all.
      let r' = PR arm (VarMap.insert c p pats) vBind tyBind
      goCtors unc (HMap.insert v' (Just cap, r':cases') cases) rs
    goCtors unc cases ((p, r):rs) =
      let r' = goGeneric p r
          cases' = HMap.map (second (r':)) cases
      in goCtors (r':unc) cases' rs

    build pats = do
      nodes <- traverse (\(p, caps, r) -> (p,) <$> lowerOne (foldr (\(Capture v ty) -> VarMap.insert v ty) tys caps) r) pats
      let atom = Ref var ty
          allArms = foldr ((<>) . freeInArm . snd) mempty nodes
      pure (ArmMatch allArms atom nodes)

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
normalisePattern (S.PRecord fs a) = S.PRecord (map (second normalisePattern) fs) a
-- Reduce these cases to something else
normalisePattern (S.PWrapper _ p _) = normalisePattern p
normalisePattern (S.PType p _ _) = normalisePattern p
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
freshFromPat (S.Capture (TvName v) _) = freshFrom (mkVal v)
freshFromPat _ = fresh ValueVar

-- | If this is a "trivial" pattern (it does not require a branch in
-- order to match).
isTrivialPat :: S.Pattern Typed -> Bool
isTrivialPat S.Wildcard{} = True
isTrivialPat S.Capture{} = True
isTrivialPat _ = False

patternVars :: Pattern CoVar -> [(CoVar, Type CoVar)]
patternVars (Destr _ p) = [captureVars p]
patternVars (PatExtend p ps) = captureVars p : map (captureVars . snd) ps
patternVars (PatValues ps) = map captureVars ps
patternVars Constr{} = []
patternVars PatLit{} = []
patternVars PatWildcard{} = []

captureVars :: Capture CoVar -> (CoVar, Type CoVar)
captureVars (Capture v ty) = (v, ty)

patternVars' :: S.Pattern Typed -> [(CoVar, Type CoVar)]
patternVars' S.Wildcard{} = []
patternVars' S.PLiteral{} = []
patternVars' (S.Capture (TvName v) (_, ty)) = [(mkVal v, lowerType ty)]
patternVars' (S.Destructure _ p _) = maybe [] patternVars' p
patternVars' (S.PRecord fs _) = concatMap (patternVars' . snd) fs
patternVars' (S.PWrapper _ p _) = patternVars' p
patternVars' (S.PSkolem p _ _) = patternVars' p
patternVars' (S.PType p _ _) = patternVars' p
patternVars' (S.PTuple ps _) = concatMap patternVars' ps
