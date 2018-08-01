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
  , ViewPatterns
  #-}
module Control.Monad.Infer
  ( module M, firstName
  , TypeError(..)
  , Constraint(..)
  , Env
  , MonadInfer, Name
  , lookupTy, lookupTy', genNameFrom, runInfer, freeInEnv
  , difference, freshTV, refreshTV
  , instantiate
  , SomeReason(..), Reasonable, propagateBlame
  , becauseExp, becausePat
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
import Data.Typeable
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
import Syntax.Implicits
import Syntax.Pretty
import Syntax.Types
import Syntax.Subst

type MonadInfer p m = (MonadError TypeError m, MonadReader Env m, MonadWriter (Seq.Seq (Constraint p)) m, MonadNamey m)

data Constraint p
  = ConUnify    SomeReason (Var p)  (Type p) (Type p)
  | ConSubsume  SomeReason (Var p)  (ImplicitScope p) (Type p) (Type p)
  | ConImplies  SomeReason (Type p) (Seq.Seq (Constraint p)) (Seq.Seq (Constraint p))
  | ConImplicit SomeReason (Var p)  (ImplicitScope p) (Type p) (Type p)
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

  Note :: (Pretty x, Typeable x) => TypeError -> x -> TypeError
  Suggestion :: Pretty x => TypeError -> x -> TypeError

  CanNotInstance :: Pretty (Var p)
                 => Type p {- record type -}
                 -> Type p {- instance -}
                 -> TypeError

  Malformed :: Pretty (Var p) => Type p -> TypeError

  -- Implicit parameters
  NoImplicit :: (Ord (Var p), Pretty (Var p)) => Type p -> (Doc -> Doc) -> TypeError
  AmbiguousType :: (Ord (Var p), Pretty (Var p)) => Var p -> Type p -> Set.Set (Var p) -> TypeError

  NotPromotable :: Pretty (Var p) => Var p -> Type p -> Doc -> TypeError
  ManyErrors :: [TypeError] -> TypeError

data WhyInstantiate = Expression | Subsumption

instance (Show (Ann p), Show (Var p), Ord (Var p), Substitutable p (Type p)) => Substitutable p (Constraint p) where
  ftv (ConUnify _ _ a b) = ftv a `Set.union` ftv b
  ftv (ConSubsume _ _ s a b) = ftv a `Set.union` ftv b <> foldMap ftv (keys s)
  ftv (ConImplies _ t a b) = ftv a `Set.union` ftv b `Set.union` ftv t
  ftv (ConImplicit _ _ s t t') = foldMap ftv (keys s) <> ftv t <> ftv t'
  ftv (ConFail _ _ t) = ftv t

  apply s (ConUnify e v a b) = ConUnify e v (apply s a) (apply s b)
  apply s (ConSubsume e v t a b) = ConSubsume e v (mapTypes (apply s) t) (apply s a) (apply s b)
  apply s (ConImplies e t a b) = ConImplies e (apply s t) (apply s a) (apply s b)
  apply s (ConImplicit e t m i t') = ConImplicit e t (mapTypes (apply s) m) (apply s i) (apply s t')
  apply s (ConFail a e t) = ConFail a e (apply s t)

instance Pretty (Var p) => Pretty (Constraint p) where
  pretty (ConUnify _ _ a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (ConSubsume _ _ _ a b) = pretty a <+> soperator (string "<=") <+> pretty b
  pretty (ConImplies _ t a b) = brackets (pretty t) <+> hsep (punctuate comma (toList (fmap pretty a)))
                            <+> soperator (char '⊃')
                            <#> indent 2 (vsep (punctuate comma (toList (fmap pretty b))))
  pretty (ConImplicit _ v _ t _) = pretty v <+> colon <+> pretty t <+> parens (keyword "implicitly")
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
  var <- refreshTV v
  let map = Map.singleton v var

      appThisTy e = ExprWrapper (TypeApp var) e (annotation e, apply map ty)
  (k, _, t) <- instantiate r (apply map ty)
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
        = Fun (PatParam (PType (Capture (TvName var) (ann, co)) co (ann, co))) (cont (App e (VarRef (TvName var) (ann, co)) (ann, od))) (ann, ty)

  pure (Just lam, tp, ty)
instantiate _ ty = pure (Just id, ty, ty)

freshTV :: MonadNamey m => m (Type Typed)
freshTV = TyVar . TvName <$> genName

refreshTV :: MonadNamey m => Var Typed -> m (Type Typed)
refreshTV (TvName v) = TyVar . TvName <$> genNameFrom nm where
  nm = case v of
    TgInternal x -> x
    TgName x _ -> x

instance Pretty TypeError where
  pretty (NotEqual b@TyArr{} a) =
    let thing = case a of
          TyType -> string "type constructor"
          _ -> string "function"
     in vcat [ string "Could not match type" <+> displayType a <+> string "with" <+> displayType b
             , string "Have you applied a" <+> thing <+> "to the wrong number of arguments?"
             ]
  pretty (NotEqual TyType b) =
    vcat [ string "Expected a type, but this has kind" <+> displayType b
         , string "Have you applied a type constructor to the wrong number of arguments?"
         ]
  pretty (NotEqual a b) = string "Could not match expected type" <+> displayType b <+> string "with" <+> displayType a

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

  pretty (SkolBinding (Skolem _ v _ m) t) =
    vsep [ string "Could not match expected type" <+> stypeSkol (squote <> pretty v) <+> "with" <+> whatIs t
         , empty
         , case m of
             ByAscription t -> bullet $ string "When checking that this expression has type" <#> indent 5 (displayType t)
             BySubsumption s t ->
               vsep [ bullet $ string "When checking that the type"
                    , indent 5 (displayType s)
                    , indent 2 (string "can be made as polymorphic as")
                    , indent 5 (displayType t)
                    ]
             ByExistential c t ->
               vsep [ bullet $ string "Where the type variable" <+> stypeSkol (pretty v) <+> "is an" <+> keyword "existential" <> comma
                    , indent 2 $ string "bound by the constructor" <+> stypeCon (pretty c) <> ", which has type"
                    , indent 5 (displayType t)
                    ]
           ]
   where whatIs (TySkol (Skolem _ v _ _)) = string "the rigid type variable" <+> stypeVar (squote <>pretty v)
         whatIs t = string "the type" <+> displayType (withoutSkol t)

  pretty (NoImplicit tau doc) =
    doc $ vsep [ "Could not choose implicit value of type" <+> displayType tau ]

  pretty (AmbiguousType v t (Set.toList -> vs)) =
    vsep [ "Ambiguous type for implicit value" <+> skeyword (pretty v)
         , bullet "Note: in the type" <+> displayType t <> comma
         , indent 4 "the type variable" <> s <+> hsep (punctuate comma (map (pretty . TyVar) vs)) <+> quan <+> "in the head" ]
    where
      s = case vs of
        [_] -> text ""
        _ -> text "s"
      quan = case vs of
        [_] -> text "is quantified but does not appear"
        _ -> text "are quantified but do not appear"

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

squish :: (a -> c) -> Maybe (c -> c) -> Maybe (a -> c)
squish f = Just . maybe f (.f)

propagateBlame :: SomeReason -> TypeError -> TypeError
propagateBlame x (ManyErrors xs) = ManyErrors (map (propagateBlame x) xs)
propagateBlame x e = ArisingFrom e x

withoutSkol :: Type p -> Type p
withoutSkol = transformType go where
  go (TySkol x) = TyVar (x ^. skolVar)
  go x = x
