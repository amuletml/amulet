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

import Data.Bifunctor
import Data.Foldable
import Data.Spanned

import Syntax.Builtin
import Syntax.Var
import Syntax

-- | Desugar a program into a more simple representation
desugarProgram :: forall m. MonadNamey m => [Toplevel Resolved] -> m [Toplevel Desugared]
desugarProgram = traverse statement where
  statement :: Toplevel Resolved -> m (Toplevel Desugared)
  statement (LetStmt vs) = LetStmt <$> traverse binding vs
  statement (Module v ss) = Module v <$> traverse statement ss
  statement (Instance a b c m d) = Instance a (ty <$> b) (ty c) <$> traverse binding m <*> pure d
  statement (Class a b c m d) = Class a (ty <$> b) (tyA <$> c) <$> traverse classItem m <*> pure d
  statement (Open v a) = pure $ Open v a
  statement (ForeignVal v x t a) = pure $ ForeignVal v x (ty t) a
  statement (TypeDecl v arg cs) = pure $ TypeDecl v (map tyA arg) (map ctor cs)

  classItem (MethodSig v t a) = pure $ MethodSig v (ty t) a
  classItem (DefaultMethod m a) = DefaultMethod <$> binding m <*> pure a

  expr :: Expr Resolved -> m (Expr Desugared)
  expr (Literal l a) = pure (Literal l a)
  expr (VarRef v a) = pure (VarRef v a)
  expr (Hole v a) = pure (Hole v a)
  expr (ExprWrapper w e a) = ExprWrapper (wrapper w) <$> expr e <*> pure a
  expr (Let vs e a) = Let <$> traverse binding vs <*> expr e <*> pure a
  expr (If c t e a) = If <$> expr c <*> expr t <*> expr e <*> pure a
  expr (App f x a) = App <$> expr f <*> expr x <*> pure a
  expr (Fun p b a) = Fun (param p) <$> expr b <*> pure a
  expr (Begin es a) = Begin <$> traverse expr es <*> pure a
  expr (Match e bs a) = Match <$> expr e <*> traverse arm bs <*> pure a
  expr (Function [Arm p Nothing b] a) = Fun (PatParam (pat p)) <$> expr b <*> pure a
  expr (Function bs a) = do
    (cap, rhs) <- fresh a
    Fun (PatParam cap) <$>
      (Match rhs <$> traverse arm bs <*> pure a)
      <*> pure a
  -- Special case @@ so we can work on skolem variables
  expr (BinOp l (VarRef v _) r a) | v == opAppName = App <$> expr l <*> expr r <*> pure a
  expr (BinOp l o r a) = BinOp <$> expr l <*> expr o <*> expr r <*> pure a
  expr (Ascription e t a) = Ascription <$> expr e <*> pure (ty t) <*> pure a
  expr (Record rs a) = Record <$> traverse field rs <*> pure a
  expr (RecordExt e rs a) = RecordExt <$> expr e <*> traverse field rs <*> pure a
  expr (Access e k a) = Access <$> expr e <*> pure k <*> pure a

  expr (LeftSection op vl an) = do
    (cap, rhs) <- fresh an
    op' <- expr op
    vl' <- expr vl
    let go lhs = Fun (PatParam cap) (BinOp rhs op' lhs an) an
    case vl' of
      VarRef{} -> pure $ go vl'
      Literal{} -> pure $ go vl'
      _ -> do
        ~(Capture lv _, ref) <- fresh an
        pure $ Let [Binding lv vl' False an] (go ref) an

  expr (RightSection vl op an) = expr (App op vl an)
  expr (BothSection o _) = expr o

  expr (AccessSection k a) = do
    (cap, ref) <- fresh a
    pure (Fun (PatParam cap) (Access ref k a) a)

  expr (Parens e _) = expr e

  expr (Tuple es a) = Tuple <$> traverse expr es <*> pure a
  expr (TupleSection es a) = do
    es' <- traverse (traverse expr) es
    (args, binds, tuple) <- foldrM (buildTuple a) ([], [], []) es'
    pure $ foldf (\(v, e) r -> Let [Binding v e False a] r a) binds
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

  expr (OpenIn _ e _) = expr e

  buildTuple :: Ann Desugared
             -> Maybe (Expr Desugared)
             -> ([Pattern Desugared], [(Var Desugared, Expr Desugared)], [Expr Desugared])
             -> m ([Pattern Desugared], [(Var Desugared, Expr Desugared)], [Expr Desugared])
  buildTuple a Nothing (as, vs, tuple) = do
    (p, v) <- fresh a
    pure (p:as, vs, v:tuple)
  buildTuple _ (Just e@VarRef{}) (as, vs, tuple) = pure (as, vs, e:tuple)
  buildTuple _ (Just e@Literal{}) (as, vs, tuple) = pure (as, vs, e:tuple)
  buildTuple a (Just e) (as, vs, tuple) = do
    ~(Capture v _, ref) <- fresh a
    pure (as, (v, e):vs, ref:tuple)

  binding (Binding v e c a) = Binding v <$> expr e <*> pure c <*> pure a

  binding (Matching (Capture v _) e a) =
    Binding v <$> expr e <*> pure True <*> pure a
  binding (Matching (PType p t _) e a) = binding (Matching p (Ascription e t (annotation e)) a)
  binding (Matching p e a) = Matching (pat p) <$> expr e <*> pure a
  binding TypedMatching{} = error "TypedMatching{} desugar binding"

  arm (Arm p g e) = Arm (pat p) <$> traverse expr g <*> expr e

  wrapper :: Wrapper Resolved -> Wrapper Desugared
  wrapper = error "Wrapper during desugaring"

  ty :: Type Resolved -> Type Desugared
  ty (TyCon v) = TyCon v
  ty (TyVar v) = TyVar v
  ty (TyPromotedCon v) = TyPromotedCon v
  ty (TyApp f x) = TyApp (ty f) (ty x)
  ty (TyPi b p) = TyPi (binder b) (ty p)
  ty (TyRows t fs) = TyRows (ty t) (map (second ty) fs)
  ty (TyExactRows fs) = TyExactRows (map (second ty) fs)
  ty (TyTuple l r) = TyTuple (ty l) (ty r)
  ty (TyWildcard t) = TyWildcard (ty <$> t)
  ty TySkol{} = error "TySkol in desugar"
  ty TyWithConstraints{} = error "TywithConstraints in desugar"
  ty (TyParens t) = ty t
  ty (TyOperator l o r) = TyOperator (ty l) o (ty r)
  ty TyType = TyType

  binder :: TyBinder Resolved -> TyBinder Desugared
  binder (Anon t) = Anon (ty t)
  binder (Implicit t) = Implicit (ty t)
  binder (Invisible v t) = Invisible v (ty <$> t)

  pat :: Pattern Resolved -> Pattern Desugared
  pat (Wildcard a) = Wildcard a
  pat (Capture v a) = Capture v a
  pat (Destructure v p a) = Destructure v (pat <$> p) a
  pat (PAs p v a) = PAs (pat p) v a
  pat (PType p t a) = PType (pat p) (ty t) a
  pat (PRecord fs a) = PRecord (map (second pat) fs) a
  pat (PTuple ts a) = PTuple (map pat ts) a
  pat (PLiteral l a) = PLiteral l a
  pat (PWrapper (w, t) p a) = PWrapper (wrapper w, ty t) (pat p) a
  pat (PSkolem p v a) = PSkolem (pat p) v a
  pat (PList ps a) = build ps where
    build [] = Destructure nILName Nothing a
    build (x:xs) =
      Destructure cONSName (Just (PTuple [pat x, build xs] a)) a

  tyA :: TyConArg Resolved -> TyConArg Desugared
  tyA (TyVarArg v) = TyVarArg v
  tyA (TyAnnArg v t) = TyAnnArg v (ty t)

  param :: Parameter Resolved -> Parameter Desugared
  param (PatParam p) = PatParam (pat p)
  param (EvParam p) = EvParam (pat p)

  field :: Field Resolved -> m (Field Desugared)
  field (Field f e a) = Field f <$> expr e <*> pure a

  ctor :: Constructor Resolved -> Constructor Desugared
  ctor (UnitCon v a) = UnitCon v a
  ctor (ArgCon v t a) = ArgCon v (ty t) a
  ctor (GeneralisedCon v t a) = GeneralisedCon v (ty t) a

  transListComp :: (Expr Resolved, [CompStmt Resolved], Ann Resolved)
                -> Expr Desugared -> m (Expr Desugared)
  transListComp (ex, CompGen v l1 _:qs, an) l2 = do
    h <- genName
    (cus, us) <- fresh an
    (cus', us') <- fresh an
    (cx, x) <- fresh an
    l1 <- expr l1
    success <- transListComp (ex, qs, an) (App (VarRef h an) us' an)
    pure $
      Let [ Binding h
              (Fun (PatParam cus)
                (Match us
                  [ Arm { armPat = consPat cx cus' an
                        , armGuard = Nothing
                        , armExp =
                            Match x
                              [ Arm { armPat = pat v
                                    , armGuard = Nothing
                                    , armExp = success }
                              , Arm { armPat = Wildcard an
                                    , armGuard = Nothing
                                    , armExp = App (VarRef h an) us' an
                                    }
                              ] an
                        }
                  , Arm { armPat = Wildcard an
                        , armGuard = Nothing
                        , armExp = l2 } ] an)
                  an) True an ]
        (App (VarRef h an) l1 an)
        an
  transListComp (ex, CompLet bs _:qs, an) l =
    Let <$> traverse binding bs <*> transListComp (ex, qs, an) l <*> pure an
  transListComp (ex, CompGuard e:qs, an) l =
    If <$> expr e <*> transListComp (ex, qs, an) l <*> pure l <*> pure an
  transListComp (ex, [], an) l = cons <$> expr ex <*> pure l <*> pure an

  consPat p ps an = Destructure cONSName (Just (PTuple [p, ps] an)) an
  cons x xs an = App (VarRef cONSName an) (Tuple [x, xs] an) an

  foldf f xs v = foldr f v xs

fresh :: MonadNamey m => Ann Desugared -> m (Pattern Desugared, Expr Desugared)
fresh an = do
  var <- genName
  pure (Capture var an, VarRef var an)
