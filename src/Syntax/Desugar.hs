{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings, TypeFamilies #-}

{- | The desugar process is run before the type checker in order to
   simplify the number of cases it needs to handle.

     * Replace sections and tuple sections with a lambda (or function
       symbol).

     * Removes 'Parens' and 'OpenIn' (these are only required by the
       resolver).

     * Replace @\@\@@ with 'App'.

     * Replace 'Function' with a lambda and match.
 -}
module Syntax.Desugar (desugarProgram) where

import Control.Monad.Namey

import Data.Text (Text)
import Data.Bifunctor
import Data.Foldable
import Data.Spanned

import Syntax.Builtin
import Syntax.Var
import Syntax

-- | Desugar a program into a more simple representation
desugarProgram :: forall m. MonadNamey m => [Toplevel Resolved] -> m [Toplevel Desugared]
desugarProgram = traverse statement

statement :: forall m. MonadNamey m => Toplevel Resolved -> m (Toplevel Desugared)
statement (LetStmt re am vs a) = LetStmt re am <$> traverse binding vs <*> pure a
statement (Module am v t) = Module am v <$> modTerm t
statement (Instance a b c m _ d) = Instance a (ty <$> b) (ty c) <$> traverse instItem m <*> pure False <*> pure d where
  instItem (MethodImpl b) = MethodImpl <$> binding b
  instItem (TypeImpl v as t a) = pure $ TypeImpl v (map tyA as) (ty t) a
statement (Class am a b c fd m d) = Class am a (ty <$> b) (tyA <$> c) (map go fd) <$> traverse classItem m <*> pure d where
  go (Fundep f t a) = Fundep f t a
statement (DeriveInstance t an) = pure $ DeriveInstance (ty t) an

statement (Open v) = Open <$> modTerm v
statement (Include v) = Include <$> modTerm v

statement (ForeignVal am v x t a) = pure $ ForeignVal am v x (ty t) a
statement (TypeDecl am v arg cs a) = pure $ TypeDecl am v (map tyA arg) (map ctor <$> cs) a
statement (TySymDecl am v arg exp a) = pure $ TySymDecl am v (map tyA arg) (ty exp) a
statement (TypeFunDecl am v arg ks bd a) = pure $ TypeFunDecl am v (map tyA arg) (fmap ty ks) (map eq bd) a where
  eq (TyFunClause f t a) = TyFunClause (ty f) (ty t) a

modTerm :: MonadNamey m => ModuleTerm Resolved -> m (ModuleTerm Desugared)
modTerm (ModStruct ss a) = ModStruct <$> traverse statement ss <*> pure a
modTerm (ModRef v a) = pure $ ModRef v a
modTerm (ModImport p a) = pure $ ModImport p a
modTerm (ModTargetImport ps a) = pure $ ModTargetImport (map targetImport ps) a where
  targetImport :: TargetImport Resolved -> TargetImport Desugared
  targetImport (TargetImport backend path a) = TargetImport backend path a

classItem :: forall m. MonadNamey m => ClassItem Resolved -> m (ClassItem Desugared)
classItem (MethodSig v t a) = pure $ MethodSig v (ty t) a
classItem (DefaultMethod m a) = DefaultMethod <$> binding m <*> pure a
classItem (AssocType v as t a) = pure $ AssocType v (map tyA as) (ty t) a

expr :: forall m. MonadNamey m => Expr Resolved -> m (Expr Desugared)
expr (Literal l a) = pure (Literal l a)
expr (VarRef v a) = pure (VarRef v a)
expr (Hole v a) = pure (Hole v a)
expr (ExprWrapper w e a) = ExprWrapper (wrapper w) <$> expr e <*> pure a
expr (Let re vs e a) = Let re <$> traverse binding vs <*> expr e <*> pure a
expr (If c t e a) = If <$> expr c <*> expr t <*> expr e <*> pure a
expr (App f x a) = App <$> expr f <*> expr x <*> pure a
expr (Fun p b a) = Fun (param p) <$> expr b <*> pure a
expr (Begin es a) = Begin <$> traverse expr es <*> pure a
expr (Match e bs p a) = Match <$> expr e <*> traverse arm bs <*> pure p <*> pure a
expr (Function bs p a) = do
  let name = case bs of
               [] -> "bot"
               [Arm b _ _ _] | Just n <- getPatternName b -> n
               _ -> "x"
  (cap, rhs) <- fresh name a
  Fun (EvParam cap) <$>
    (Match rhs <$> traverse arm bs <*> pure p <*> pure a)
    <*> pure a
-- Special case @@ so we can work on skolem variables
expr (BinOp l (VarRef v _) r a) | v == opAppName = App <$> expr l <*> expr r <*> pure a
expr (BinOp l o r a) = BinOp <$> expr l <*> expr o <*> expr r <*> pure a
expr (Ascription e t a) = Ascription <$> expr e <*> pure (ty t) <*> pure a
expr (Record rs a) = Record <$> traverse field rs <*> pure a
expr (RecordExt e rs a) = RecordExt <$> expr e <*> traverse field rs <*> pure a
expr (Access e k a) = Access <$> expr e <*> pure k <*> pure a

expr (LeftSection op vl an) = do
  (cap, rhs) <- fresh "r" an
  op' <- expr op
  vl' <- expr vl
  let go lhs = Fun (PatParam cap) (BinOp rhs op' lhs an) an
  case vl' of
    VarRef{} -> pure $ go vl'
    Literal{} -> pure $ go vl'
    _ -> do
      ~(Capture lv _, ref) <- fresh "x" an
      pure $ Let NonRecursive [Binding lv vl' False an] (go ref) an

expr (RightSection vl op an) = expr (App op vl an)
expr (BothSection o _) = expr o

expr (AccessSection k a) = do
  (cap, ref) <- fresh "tbl" a
  pure (Fun (PatParam cap) (Access ref k a) a)

expr (Parens e _) = expr e

expr (Tuple es a) = Tuple <$> traverse expr es <*> pure a
expr (TupleSection es a) = do
  es' <- traverse (traverse expr) es
  (args, binds, tuple) <- foldrM (buildTuple a) ([], [], []) es'
  pure $ foldf (\(v, e) r -> Let NonRecursive [Binding v e False a] r a) binds
       $ foldf (\v e -> Fun (PatParam v) e a) args
       $ Tuple tuple a

expr (Lazy e a) = do
  e <- expr e
  pure $ App (VarRef lAZYName a)
             (Fun (PatParam (PLiteral LiUnit a)) e a)
             a

expr (Vta e t a) = Vta <$> expr e <*> pure (ty t) <*> pure a
expr (ListExp e t) = ListExp <$> traverse expr e <*> pure t
expr (ListComp _ [] an) = pure $ ListExp [] an
expr (ListComp e qs an) = transListComp (e, qs, an) (ListExp [] an)
expr (DoExpr bind qs an) = begin <$> transDoExpr (VarRef bind an) qs where
  begin = flip Begin an . (:[])

expr (Idiom pure_v app_v fn an) = Idiom pure_v app_v <$> expr fn <*> pure an
expr (ListFrom r_v x an) = ListFrom r_v <$> expr x <*> pure an
expr (ListFromTo r_v x y an) = ListFromTo r_v <$> expr x <*> expr y <*> pure an
expr (ListFromThen r_v x y an) = ListFromThen r_v <$> expr x <*> expr y <*> pure an
expr (ListFromThenTo r_v x y z an) =
  ListFromThenTo r_v <$> expr x <*> expr y <*> expr z <*> pure an

expr (OpenIn mod e an) = OpenIn <$> modTerm mod <*> expr e <*> pure an

buildTuple :: forall m. MonadNamey m => Ann Desugared
           -> Maybe (Expr Desugared)
           -> ([Pattern Desugared], [(Var Desugared, Expr Desugared)], [Expr Desugared])
           -> m ([Pattern Desugared], [(Var Desugared, Expr Desugared)], [Expr Desugared])
buildTuple a Nothing (as, vs, tuple) = do
  (p, v) <- fresh "a" a
  pure (p:as, vs, v:tuple)
buildTuple _ (Just e@VarRef{}) (as, vs, tuple) = pure (as, vs, e:tuple)
buildTuple _ (Just e@Literal{}) (as, vs, tuple) = pure (as, vs, e:tuple)
buildTuple a (Just e) (as, vs, tuple) = do
  ~(Capture v _, ref) <- fresh "x" a
  pure (as, (v, e):vs, ref:tuple)

binding :: forall m. MonadNamey m => Binding Resolved -> m (Binding Desugared)
binding (Binding v e c a) = Binding v <$> expr e <*> pure c <*> pure a

binding (Matching (Capture v _) e a) =
  Binding v <$> expr e <*> pure True <*> pure a
binding (Matching (PType p t _) e a) = binding (Matching p (Ascription e t (annotation e)) a)
binding (Matching p e a) = Matching (pat p) <$> expr e <*> pure a
binding TypedMatching{} = error "TypedMatching{} desugar binding"

arm :: forall m. MonadNamey m => Arm Resolved -> m (Arm Desugared)
arm (Arm p g e a) = Arm (pat p) <$> traverse expr g <*> expr e <*> pure a

wrapper :: Wrapper Resolved -> Wrapper Desugared
wrapper = error "Wrapper during desugaring"

ty :: Type Resolved -> Type Desugared
ty (TyCon v _) = TyCon v ()
ty (TyLit v) = TyLit v
ty (TyVar v _) = TyVar v ()
ty (TyPromotedCon v _) = TyPromotedCon v ()
ty (TyApp f x) = TyApp (ty f) (ty x)
ty (TyPi b p) = TyPi (binder b) (ty p)
ty (TyRows t fs) = TyRows (ty t) (map (second ty) fs)
ty (TyExactRows fs) = TyExactRows (map (second ty) fs)
ty (TyTuple l r) = TyTuple (ty l) (ty r)
ty (TyTupleL l r) = TyTupleL (ty l) (ty r)
ty (TyWildcard t) = TyWildcard (ty <$> t)
ty TySkol{} = error "TySkol in desugar"
ty TyWithConstraints{} = error "TywithConstraints in desugar"
ty (TyParens t) = ty t
ty (TyOperator l o r)
  | TyCon o' _ <- o, o' == tyTupleName = TyTuple (ty l) (ty r)
  | otherwise = TyApp (TyApp (ty o) (ty l)) (ty r)

ty TyType = TyType

binder :: TyBinder Resolved -> TyBinder Desugared
binder (Anon t) = Anon (ty t)
binder (Implicit t) = Implicit (ty t)
binder (Invisible v t req) = Invisible v (ty <$> t) req

pat :: Pattern Resolved -> Pattern Desugared
pat (Wildcard a) = Wildcard a
pat (Capture v a) = Capture v a
pat (Destructure v p a) = Destructure v (pat <$> p) a
pat (PAs p v a) = PAs (pat p) v a
pat (PType p t a) = PType (pat p) (ty t) a
pat (PRecord fs a) = PRecord (map (second pat) fs) a
pat (PTuple ts a) = PTuple (map pat ts) a
pat (PLiteral l a) = PLiteral l a
pat PGadtCon{} = undefined
pat (PList ps a) = build ps where
  build [] = Destructure nILName Nothing a
  build (x:xs) =
    Destructure cONSName (Just (PTuple [pat x, build xs] a)) a

tyA :: TyConArg Resolved -> TyConArg Desugared
tyA (TyVarArg v) = TyVarArg v
tyA (TyAnnArg v t) = TyAnnArg v (ty t)
tyA (TyInvisArg v t) = TyInvisArg v (ty t)

param :: Parameter Resolved -> Parameter Desugared
param (PatParam p) = PatParam (pat p)
param (EvParam p) = EvParam (pat p)

field :: forall m. MonadNamey m => Field Resolved -> m (Field Desugared)
field (Field f e a) = Field f <$> expr e <*> pure a

ctor :: Constructor Resolved -> Constructor Desugared
ctor (UnitCon ac v a) = UnitCon ac v a
ctor (ArgCon ac v t a) = ArgCon ac v (ty t) a
ctor (GadtCon ac v t a) = GadtCon ac v (ty t) a

transListComp :: forall m. MonadNamey m
              => (Expr Resolved, [CompStmt Resolved], Ann Resolved)
              -> Expr Desugared -> m (Expr Desugared)
transListComp (ex, CompGen v l1 an:qs, an') l2 = do
  h <- genName
  (cus, us) <- fresh "xss" an
  (cus', us') <- fresh "xs" an
  (cx, x) <- fresh "x" an
  l1 <- expr l1
  success <- transListComp (ex, qs, an) (App (VarRef h an) us' an)
  pure $
    Let NonRecursive
        [ Binding h
            (Fun (PatParam cus)
              (Match us
                [ Arm { armPat = consPat cx cus' an
                      , armGuard = Nothing
                      , armAnn = an
                      , armExp =
                          maybeMatch x
                             ( Arm { armPat = pat v
                                   , armGuard = Nothing
                                   , armExp = success
                                   , armAnn = an }
                             : [ Arm { armPat = Wildcard an
                                     , armGuard = Nothing
                                     , armExp = App (VarRef h an) us' an
                                     , armAnn = an
                                     }
                               | refutable (pat v) ] )
                            an
                      }
                , Arm { armPat = Wildcard an
                      , armGuard = Nothing
                      , armExp = l2
                      , armAnn = an} ] an an)
                an) True an ]
      (App (VarRef h an) l1 an)
      an'
transListComp (ex, CompLet bs _:qs, an) l =
  Let NonRecursive <$> traverse binding bs <*> transListComp (ex, qs, an) l <*> pure an
transListComp (ex, CompGuard e:qs, an) l =
  If <$> expr e <*> transListComp (ex, qs, an) l <*> pure l <*> pure an
transListComp (ex, [], an) l = cons <$> expr ex <*> pure l <*> pure an

maybeMatch :: Expr Desugared -> [Arm Desugared] -> Ann Desugared -> Expr Desugared
maybeMatch ex [arm@Arm{ armGuard = Nothing, armExp = body }] ann =
  Let NonRecursive [Matching (armPat arm) ex ann] body ann
maybeMatch ex arms ann = Match ex arms ann ann

transDoExpr :: forall m. MonadNamey m => Expr Desugared -> [CompStmt Resolved] -> m (Expr Desugared)
transDoExpr bind = go where
  go (CompGen p e an:qs) = do
    e <- expr e
    cont <- Fun (PatParam (pat p)) <$> go qs <*> pure an
    pure $ BinOp e bind cont an
  go (CompLet bg an:qs) = Let NonRecursive <$> traverse binding bg <*> go qs <*> pure an
  go [CompGuard e] = expr e
  go (CompGuard e:qs) = do
    let an = annotation e
    e <- expr e
    cont <- Fun (PatParam (Wildcard an)) <$> go qs <*> pure an
    pure $ BinOp e bind cont an
  go [] = undefined

consPat :: (Var p ~ VarResolved) => Pattern p -> Pattern p -> Ann p -> Pattern p
consPat p ps an = Destructure cONSName (Just (PTuple [p, ps] an)) an

cons :: (Var p ~ VarResolved) => Expr p -> Expr p -> Ann p -> Expr p
cons x xs an = App (VarRef cONSName an) (Tuple [x, xs] an) an

foldf :: Foldable t => (a -> b -> b) -> t a -> b -> b
foldf f xs v = foldr f v xs

refutable :: Pattern Desugared -> Bool
refutable Wildcard{} = False
refutable Capture{} = False
refutable Destructure{} = True
refutable PList{} = True
refutable PLiteral{} = True
refutable (PAs p _ _) = refutable p
refutable (PType p _ _) = refutable p
refutable (PRecord rs _) = any (refutable . snd) rs
refutable (PTuple ps _) = any refutable ps
refutable PGadtCon{} = undefined

fresh :: MonadNamey m => Text -> Ann Desugared -> m (Pattern Desugared, Expr Desugared)
fresh n an = do
  var <- genNameFrom n
  pure (Capture var an, VarRef var an)

getPatternName :: Pattern Resolved -> Maybe Text
getPatternName (Capture (TgName n _) _) = Just n
getPatternName (PAs _ (TgName n _) _) = Just n
getPatternName (PType p _ _) = getPatternName p
getPatternName _ = Nothing
