{-# LANGUAGE FlexibleContexts
  , UndecidableInstances
  , FlexibleInstances
  , GADTs
  , ConstraintKinds
  , StandaloneDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , ScopedTypeVariables
  , RecordWildCards
  #-}
module Control.Monad.Infer
  ( module M, firstName
  , TypeError(..)
  , Constraint(..)
  , Env
  , MonadInfer, Name
  , lookupTy, lookupTy', genNameFrom, runInfer, freeInEnv
  , difference, freshTV
  , instantiate
  , SomeReason(..), Reasonable, propagateBlame
  , WhyInstantiate(..)

  -- lenses:
  , names, typeVars
  )
  where

import Control.Monad.Writer.Strict as M hiding ((<>))
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Namey as M
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Bifunctor
import Data.Function
import Data.Foldable
import Data.Spanned
import Data.Triple
import Data.Reason
import Data.Maybe
import Data.Text (Text)
import Data.List

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Transform
import Syntax.Pretty
import Syntax.Types
import Syntax.Subst

type MonadInfer p m = (MonadError TypeError m, MonadReader Env m, MonadWriter (Seq.Seq (Constraint p)) m, MonadNamey m)

data Constraint p
  = ConUnify   SomeReason (Var p)  (Type p) (Type p)
  | ConSubsume SomeReason (Var p)  (Type p) (Type p)
  | ConImplies SomeReason (Type p) (Seq.Seq (Constraint p)) (Seq.Seq (Constraint p))
  | ConImplicit SomeReason (Var p) (Map.Map (Type p) (Var p)) (Type p)
  | ConFail (Ann p) (Var p) (Type p) -- for holes. I hate it.

deriving instance (Show (Ann p), Show (Var p), Show (Expr p), Show (Type p))
  => Show (Constraint p)

deriving instance (Eq (Ann p), Eq (Var p), Eq (Expr p), Eq (Type p))
  => Eq (Constraint p)

data TypeError where
  NotEqual :: (Pretty (Var p), Ord (Var p)) => Type p -> Type p -> TypeError
  Occurs :: (Pretty (Var p), Ord (Var p)) => Var p -> Type p -> TypeError

  NotInScope :: Var Resolved -> TypeError
  FoundHole :: Var Typed -> Type Typed -> TypeError

  Impredicative :: (Pretty (Var p), Ord (Var p)) => Var p -> Type p -> TypeError
  ImpredicativeApp :: (Pretty (Var p), Ord (Var p)) => Type p -> Type p -> TypeError

  EscapedSkolems :: [Skolem Typed] -> Type Typed -> TypeError
  SkolBinding :: Skolem Typed -> Type Typed -> TypeError

  ArisingFrom :: TypeError -> SomeReason -> TypeError

  NoOverlap :: Type Typed -> Type Typed -> TypeError

  Note :: Pretty x => TypeError -> x -> TypeError
  Suggestion :: Pretty x => TypeError -> x -> TypeError

  CanNotInstance :: Pretty (Var p)
                 => Type p {- record type -}
                 -> Type p {- instance -}
                 -> TypeError

  Malformed :: Pretty (Var p) => Type p -> TypeError

  -- Visible quantification
  WrongQuantifier   :: (Pretty (Var p), Pretty (Var p'), Ord (Var p), Ord (Var p')) => Expr p -> Type p' -> TypeError
  NakedInstArtifact :: (Pretty (Var p), Ord (Var p)) => Expr p -> TypeError

  -- Implicit parameters
  NoImplicit :: (Ord (Var p), Pretty (Var p)) => Type p -> (Doc -> Doc) -> TypeError

  NotPromotable :: Pretty (Var p) => Var p -> Type p -> Doc -> TypeError
  ManyErrors :: [TypeError] -> TypeError

data WhyInstantiate = Expression | Subsumption

instance (Ord (Var p), Substitutable p (Type p)) => Substitutable p (Constraint p) where
  ftv (ConUnify _ _ a b) = ftv a `Set.union` ftv b
  ftv (ConSubsume _ _ a b) = ftv a `Set.union` ftv b
  ftv (ConImplies _ t a b) = ftv a `Set.union` ftv b `Set.union` ftv t
  ftv (ConImplicit _ _ s t) = Map.foldMapWithKey (\k _ -> ftv k) s <> ftv t
  ftv (ConFail _ _ t) = ftv t

  apply s (ConUnify e v a b) = ConUnify e v (apply s a) (apply s b)
  apply s (ConSubsume e v a b) = ConSubsume e v (apply s a) (apply s b)
  apply s (ConImplies e t a b) = ConImplies e (apply s t) (apply s a) (apply s b)
  apply s (ConImplicit e t m i) = ConImplicit e t (Map.mapKeys (apply s) m) (apply s i)
  apply s (ConFail a e t) = ConFail a e (apply s t)

instance Pretty (Var p) => Pretty (Constraint p) where
  pretty (ConUnify _ _ a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (ConSubsume _ _ a b) = pretty a <+> soperator (string "<=") <+> pretty b
  pretty (ConImplies _ t a b) = brackets (pretty t) <+> hsep (punctuate comma (toList (fmap pretty a)))
                            <+> soperator (char '⊃')
                            <#> indent 2 (vsep (punctuate comma (toList (fmap pretty b))))
  pretty (ConImplicit _ v _ t) = pretty v <+> " : " <+> pretty t <+> parens (keyword "implicitly")
  pretty ConFail{} = string "fail"


lookupTy :: (MonadError TypeError m, MonadReader Env m, MonadNamey m) => Var Resolved -> m (Type Typed)
lookupTy x = do
  rs <- view (names . at x)
  case rs of
    Just t -> thd3 <$> instantiate Expression t
    Nothing -> throwError (NotInScope x)

lookupTy' :: (MonadError TypeError m, MonadReader Env m, MonadNamey m) => Var Resolved
          -> m (Maybe (Expr Typed -> Expr Typed), Type Typed, Type Typed)
lookupTy' x = do
  rs <- view (names . at x)
  case rs of
    Just t -> instantiate Expression t
    Nothing -> throwError (NotInScope x)

runInfer :: MonadNamey m
         => Env
         -> ReaderT Env (WriterT (Seq.Seq (Constraint p)) (ExceptT TypeError m)) a
         -> m (Either [TypeError] (a, Seq.Seq (Constraint p)))
runInfer ct ac = first unwrap <$> runExceptT (runWriterT (runReaderT ac ct))
  where unwrap (ManyErrors es) = concatMap unwrap es
        unwrap e = [e]

genNameFrom :: MonadNamey m => Text -> m (Var Resolved)
genNameFrom t = do
  TgName _ n <- genName
  pure (TgName t n)

firstName :: Var Resolved
firstName = TgName "a" 0

instantiate :: MonadNamey m
            => WhyInstantiate
            -> Type Typed
            -> m ( Maybe (Expr Typed -> Expr Typed)
                 , Type Typed
                 , Type Typed)
instantiate r tp@(TyPi (Invisible v _) ty) = do
  TgName _ num <- genName
  var <- pure . TyVar . TvName $ case unTvName v of
    TgInternal n -> TgName n num
    TgName n _ -> TgName n num
  let map = Map.singleton v var

      appThisTy e = ExprWrapper (TypeApp var) e (annotation e, apply map ty)
  (k, _, t) <- instantiate r (apply map ty)
  pure (squish appThisTy k, tp, t)

instantiate Expression tp@(TyPi Explicit{} _) = pure (Just id, tp, tp) -- nope!
instantiate Subsumption tp@(TyPi (Explicit v k) ty) = do
  TgName _ num <- genName
  var <- pure . TyVar . TvName $ case unTvName v of
    TgInternal n -> TgName n num
    TgName n _ -> TgName n num
  let map = Map.singleton v var
      appThisTy e = App e (InstType var (annotation e, k)) (annotation e, apply map ty)

  (k, _, t) <- instantiate Subsumption (apply map ty)
  pure (squish appThisTy k, tp, t)

instantiate r tp@(TyPi (Anon co) od@dm) = do
  (wrap, _, dm) <- instantiate r dm
  let cont = fromMaybe id wrap
  var <- genName
  let ty = TyPi (Anon co) dm
      lam :: Expr Typed -> Expr Typed
      lam e | od == dm = e
      lam e
        | ann <- annotation e
        = Fun (PType (Capture (TvName var) (ann, co)) co (ann, co)) (cont (App e (VarRef (TvName var) (ann, co)) (ann, od))) (ann, ty)

  pure (Just lam, tp, ty)
instantiate _ ty = pure (Just id, ty, ty)

freshTV :: MonadNamey m => m (Type Typed)
freshTV = TyVar . TvName <$> genName

instance Pretty TypeError where
  pretty (NotEqual b@TyArr{} a) =
    let thing = case a of
          TyType -> string "type constructor"
          _ -> string "function"
     in vcat [ string "Could not match type" <+> displayType a <+> string "with" <+> displayType b
             , string "Have you applied a" <+> thing <+> "to the wrong number of arguments?"
             ]
  pretty (NotEqual TyType b) =
    vcat [ string "Expected a type, but this annotation is of kind" <+> displayType b
         , string "Have you applied a type constructor to the wrong number of arguments?"
         ]
  pretty (NotEqual a b) = string "Could not match type" <+> displayType a <+> string "with" <+> displayType b

  pretty (Occurs v t) = string "Occurs check:" <+> string "The type variable" <+> stypeVar (pretty v) </> indent 4 (string "occurs in the type" <+> displayType t)
  pretty (NotInScope e) = string "Variable not in scope:" <+> pretty e
  pretty (ArisingFrom er ex) = pretty er <#> empty <#> nest 4 (string "Arising in" <+> blameOf ex)
  pretty (FoundHole e s) = string "Found typed hole" <+> pretty e <+> "of type" <+> displayType s

  pretty (Note te m) = pretty te <#> note <+> align (pretty m)
  pretty (Suggestion te m) = pretty te <#> bullet (string "Suggestion:") <+> align (pretty m)
  pretty (CanNotInstance rec new) = string "Can not instance hole of record type" <+> align (verbatim rec </> string " to type " <+> verbatim new)
  pretty (Malformed tp) = string "The type" <+> verbatim tp <+> string "is malformed."
  pretty (ManyErrors es) = vsep (map pretty es)

  pretty (NoOverlap ta tb)
    | TyExactRows ra <- ta
    , TyRows _ rb <- tb
    =   string "No overlap between exact record" <+> nest 9 (displayType ta </> string "and polymorphic record" <+> displayType tb)
    <#> missing ra rb
    | TyExactRows rb <- tb
    , TyRows _ ra <- ta
    =   string "No overlap between polymorphic record" <+> nest 21 (displayType ta </> string "and exact record" <+> displayType tb)
    <#> missing ra rb
    | TyExactRows ra <- ta
    , TyExactRows rb <- tb
    =  string "No overlap between exact records" <+> nest 29 (displayType ta </> string "and" <+> displayType tb)
    <#> missing rb ra
    | otherwise
    = string "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap" <+> displayType ta <+> displayType tb

  pretty (Impredicative v t)
    = vsep [ string "Illegal instantiation of type variable" <+> stypeVar (pretty v)
           , indent 16 (string "with polymorphic type" <+> displayType t)
           , note <+> string "doing so would constitute" <+> stypeCon (string "impredicative polymorphism")
           ]

  pretty (ImpredicativeApp tf tx)
    = vsep [ string "Illegal use of polymorphic type" <+> displayType tx
           , indent 2 $ string "as argument to the type function" <+> displayType tf
           , note <+> string "instantiating a type variable"
           <+> nest 2 (parens (string "the argument to" <+> displayType tf)
                   </> string "with a polymorphic type constitutes" <+> stypeCon (string "impredicative polymorphism"))
           ]

  pretty (EscapedSkolems esc t) =
    vsep [ case esc of
            [Skolem{..}] ->
              let skol = stypeVar (pretty _skolVar) in
              string "Rigid type variable" <+> skol <+> string "has escaped its scope of" <+> displayType _skolScope
                  <#> note <+> string "the variable" <+> skol <+> string "was rigidified because"
                        <+> nest 8 (prettyMotive _skolMotive <> comma)
            _ -> foldr ((<#>) . pretty . flip EscapedSkolems t . pure) empty esc
         , empty -- a line break
         , note <+> string "in type" <+> verbatim (withoutSkol t)
         ]

  pretty (NotPromotable c x err) =
    vsep [ string "The constructor" <+> pretty c <+> string "can not be used as a type"
         , note <+> "because its kind,"
         , indent 2 (pretty x)
         , err
         ]

  pretty (SkolBinding (Skolem ok v _ m) b) =
    vsep [ string "Could not match rigid type variable" <+> skol <+> string "with" <+> whatIs b
         , note <+> "the variable" <+> skol <+> string "was rigidified because"
                <+> nest 8 (prettyMotive m)
         ] <#> case b of
             TySkol (Skolem k v _ m) ->
               vcat [ indent 8 (string "and is represented by the constant") <+> stypeSkol (pretty ok)
                    , empty
                    , vsep [ note <+> "the rigid type variable" <+> stypeVar (pretty v) <> comma <+> string "in turn" <> comma
                           , indent 8 . align $ string "was rigidified because" <+> prettyMotive m
                           , indent 8 (string "and is represented by the constant") <+> stypeSkol (pretty k) ] ]
             _ -> empty
    where whatIs (TySkol (Skolem _ v _ _)) = string "the rigid type variable" <+> stypeVar (pretty v)
          whatIs t = string "the type" <+> displayType (withoutSkol t)
          skol = stypeVar (pretty v)

  pretty (WrongQuantifier _ ty@(TyPi Explicit{} _)) =
    vsep [ string "Expression given as argument to function of type" <+> displayType ty
         , indent 4 $ string "This function expects a type as its first argument;"
         , indent 4 $ string "Have you forgotten an instantiation?"
         , empty
         , note <+> "You can use a hole like"
             <+> pretty (InstHole undefined :: Expr Typed) <+> "to make the compiler infer this"
         ]
  pretty (WrongQuantifier t ty@TyArr{}) =
    vsep [ thing <+> "given as argument to function of type" <+> displayType ty
         , string "Have you applied a function to the wrong number of arguments?"
         ] where
      thing = case t of
        InstHole{} -> highlight "Hole" <+> pretty t
        InstType{} -> highlight "Type" <+> pretty t
        _ -> error "WrongQuantifier wrong"
  pretty (WrongQuantifier t ty) =
    vsep [ thing <+> "given as argument to expression of type" <+> displayType ty
         , string "Have you applied a function to too many arguments?"
         ] where
      thing = case t of
        InstHole{} -> highlight "Hole" <+> pretty t
        InstType{} -> highlight "Type" <+> pretty t
        _ -> highlight "Expression"
  pretty (NoImplicit tau doc) =
    doc $ vsep [ "Could not find implicit value of type" <+> displayType tau ]

  pretty (NakedInstArtifact h@InstHole{}) =
    vsep [ string "Instantiation hole" <+> pretty h <+> "used outside of type application" ]
  pretty (NakedInstArtifact h@InstType{}) =
    vsep [ string "Can not use the type" <+> pretty h <+> "outside of a type application" ]
  pretty (NakedInstArtifact _) = error "NakedInstArtifact wrong"

instance Spanned TypeError where
  annotation (ArisingFrom e@ArisingFrom{} _) = annotation e
  annotation (ArisingFrom _ x) = annotation x
  annotation _ = undefined

instance Note TypeError Style where
  diagnosticKind _ = ErrorMessage

  formatNote f (ArisingFrom e@ArisingFrom{} _) = formatNote f e
  formatNote f x = indent 2 (Right <$> pretty x) <#> f [annotation x]

missing :: [(Text, b)] -> [(Text, b)] -> Doc
missing ra rb
  | length ra < length rb
  =  bullet (string "Namely, the following fields are missing:") <+> hsep (punctuate comma (diff rb ra))
  | length ra > length rb
  =  bullet (string "Namely, the following fields should not be present:") <+> hsep (punctuate comma (diff ra rb))
  | length ra == length rb
  = vsep [ bullet (string "Note: no fields match")
         , bullet (string "The following fields are missing:")
            <+> hsep (punctuate comma (diff ra rb))]
missing _ _ = undefined -- freaking GHC

diff :: [(Text, b)] -> [(Text, b)] -> [Doc]
diff ra rb = map (stypeVar . string . T.unpack . fst) (deleteFirstsBy ((==) `on` fst) rb ra)

prettyMotive :: SkolemMotive Typed -> Doc
prettyMotive (ByAscription t) = string "of the context, the type" <#> displayType t
prettyMotive (BySubsumption t1 t2) = string "of a requirement that" <+> displayType t1 <#> string "be as polymorphic as" <+> displayType t2
prettyMotive (ByExistential v t) = string "it is an existential" <> comma <#> string "bound by the type of" <+> pretty v <> comma <+> displayType t

squish :: (a -> c) -> Maybe (c -> c) -> Maybe (a -> c)
squish f = Just . maybe f (.f)

propagateBlame :: SomeReason -> TypeError -> TypeError
propagateBlame x (ManyErrors xs) = ManyErrors (map (propagateBlame x) xs)
propagateBlame x e = ArisingFrom e x

withoutSkol :: Type p -> Type p
withoutSkol = transformType go where
  go (TySkol x) = TyVar (x ^. skolVar)
  go x = x
