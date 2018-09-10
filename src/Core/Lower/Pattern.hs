{-# LANGUAGE OverloadedStrings, TupleSections, ScopedTypeVariables, FlexibleContexts #-}

{-| Lowers "Syntax"'s nested patterns into "Core"'s flattened patterns.

  This (hopefully) uses the algorithm as described in "Compiling Pattern
  Matching to good Decision Trees".[1]

  [1]: https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf
-}
module Core.Lower.Pattern
  ( lowerMatch
  , lowerMatch'
  , patternTyvars'
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
import Data.Triple
import Data.Maybe

import qualified Core.Core as C
import Core.Types (unify, approximateAtomType, approximateType)
import Core.Optimise (substituteInType, fresh, freshFrom)
import Core.Lower.Basic
import Core.Builtin
import Core.Core
import Core.Var

import qualified Syntax as S
import Syntax.Var (Var(..), Resolved, Typed)
import Syntax.Pretty()

type ArmId = Int
type ArmSet = HSet.HashSet ArmId
type ArmMap = HMap.HashMap ArmId

-- | A node in the pattern matching tree
data ArmNode
  -- | A leaf in a pattern matching tree.
  = ArmLeaf
    { leafArm      :: ArmId
    , leafVarBinds :: [(CoVar, CoVar, Type CoVar)]
    , leafTyBinds  :: [(CoVar, CoVar, Type CoVar)] }
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
    , rowVarBinds :: [(CoVar, CoVar, Type CoVar)]
    , rowTyBinds  :: [(CoVar, CoVar, Type CoVar)] }
  deriving (Show)

data ArmBody m
  = BodyOnce (Term CoVar) [(CoVar, Type CoVar)]
  | BodyLambda ([(CoVar, CoVar, Type CoVar)] -> m (Term CoVar))

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

  where makeBody (p, t) = BodyOnce t (patternVars' p)

flattenResult :: forall m. MonadLower m => ArmMap (ArmBody m) -> ArmNode -> m (Term CoVar)
flattenResult bodies (ArmLeaf n vs _) =
  case bodies HMap.! n of
    BodyLambda f -> f vs
    BodyOnce bod _ ->
        pure
        -- TODO: Substitute types
      $ foldr (\(f, t, ty) -> C.Let (One (t, ty, Atom (Ref f ty)))) bod vs

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
        BodyOnce Atom{} _ -> pure (bods, build)
        BodyOnce bod fv -> do
          v <- fresh ValueVar
          a <- fresh ValueVar
          case fv of
            [] ->
              let ty = ForallTy Irrelevant tyUnit (fromJust (approximateType bod))
              in pure ( HMap.insert n (BodyLambda (\_ -> pure (App (Ref v ty) (Lit Unit)))) bods
                      , Let (One (v, ty, Lam (TermArgument a tyUnit) bod)) . build )
            [(a', aty)] ->
              let ty = ForallTy Irrelevant aty (fromJust (approximateType bod))
              in pure ( HMap.insert n (BodyLambda (\_ -> pure (App (Ref v ty) (Ref a' ty)))) bods
                      , Let (One (v, ty, Lam (TermArgument a aty) bod)) . build )
            _ -> do
              let aty = ValuesTy (map snd fv)
                  ty = ForallTy Irrelevant aty (fromJust (approximateType bod))

                  bod' = Match (Ref a aty)
                    [ Arm { _armPtrn = PatValues (map (uncurry Capture) fv)
                          , _armTy = aty
                          , _armBody = bod
                          , _armVars = fv
                          , _armTyvars = [] } ]

                  genApp vs = do
                    tvar <- fresh ValueVar
                    let tup = Values . flip map fv $ \(v, _) ->
                          let Just (v', _, ty') = find ((==v) . snd3) vs
                          in Ref v' ty'
                    pure . Let (One (tvar, aty, tup)) $ App (Ref v ty) (Ref tvar aty)

              pure ( HMap.insert n (BodyLambda genApp) bods
                   , Let (One (v, ty, Lam (TermArgument a aty) bod')) . build )

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
    addTrivial v (S.Capture (TvName v') (_, ty)) o = ((v, mkVal v', lowerType ty):) <$> o
    addTrivial _ S.Wildcard{} o = o
    addTrivial _ _ _ = Nothing
lowerOne tys rs =
  let v = findBest neededPrefix Nothing vars
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

    -- | Compute the needed prefix heuristic for a given variable. This
    -- scores a column based on how many of the leading rows requires it.
    neededPrefix :: CoVar -> Int
    neededPrefix var = go 0 rs where
      go n (PR _ ps _ _:rs)
        | Just p <- VarMap.lookup var ps
        , not (isTrivialPat p)
        = go (n + 1) rs
      go n _ = n

    -- TODO: We should really add the "Small branching factor" and "Arity" checks
    -- (namely

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
      = PR arm ps ((var, mkVal v, ty):vBind) tyBind
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
normalisePattern (S.GadtPat p _ _) = normalisePattern p -- TODO: This!
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
patternVars' (S.GadtPat p _ _) = patternVars' p
patternVars' (S.PType p _ _) = patternVars' p
patternVars' (S.PTuple ps _) = concatMap patternVars' ps

patternTyvars' :: Map.Map (Var Resolved) (Type CoVar) -> S.Pattern Typed -> [(CoVar, Type CoVar)]
patternTyvars' s = go where
  go (S.Capture _ _) = []
  go (S.Wildcard _) = []
  go (S.Destructure _ Nothing _) = []
  go (S.Destructure (TvName p) (Just t) (_, _)) =
    let tty' = lowerType (S.getType t)

        Just (skolm, ctty, _) = rootType mempty <$> Map.lookup p s
        Just uni =  unify ctty tty'
    in mapMaybe (extS skolm) (VarMap.toList uni) ++ go t
  go (S.PType p _ _) = go p
  go (S.PRecord xs _) = concatMap (go . snd) xs
  go (S.PTuple xs _) = concatMap go xs
  go (S.PLiteral _ _) = []
  go (S.PWrapper _ p _) = go p
  go (S.GadtPat p _ _) = go p

  rootType fs (ForallTy Irrelevant f c) =
    let d = VarSet.difference (freeInTy f) (freeInTy c)
        skolem = Map.filterWithKey (\v _ -> v `VarSet.member` d) fs
    in (skolem, f, c)
  rootType fs (ForallTy (Relevant v) f r) = rootType (Map.insert v f fs) r
  rootType _ _ = error "impossible constructor"

  extS sk (v, VarTy t) = (t,) <$> Map.lookup v sk
  extS _ _ = Nothing
