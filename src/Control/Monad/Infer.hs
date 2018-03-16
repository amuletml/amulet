{-# LANGUAGE FlexibleContexts
  , UndecidableInstances
  , FlexibleInstances
  , GADTs
  , ConstraintKinds
  , StandaloneDeriving
  , MultiParamTypeClasses
  , OverloadedStrings
  , TemplateHaskell
  , ScopedTypeVariables
  , RecordWildCards
  #-}
module Control.Monad.Infer
  ( module M
  , TypeError(..)
  , Constraint(..)
  , Env(..)
  , MonadInfer
  , lookupTy, lookupTy', fresh, freshFrom, runInfer, extend, freeInScope
  , extendKind, extendMany, extendManyK
  , difference, freshTV, freshKV
  , instantiate
  , SomeReason(..), Reasonable

  -- lenses:
  , values, types, typeVars
  )
  where

import Control.Monad.Writer.Strict as M hiding ((<>))
import Control.Monad.Infer.Error
import Control.Monad.Reader as M
import Control.Monad.Except as M
import Control.Monad.Gen as M
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Semigroup hiding (diff)
import Data.Function
import Data.Spanned
import Data.Triple
import Data.Text (Text)
import Data.List

import Pretty

import Syntax.Pretty
import Syntax.Subst

type MonadInfer p m = (MonadError TypeError m, MonadReader Env m, MonadWriter [Constraint p] m, MonadGen Int m)

data Env
  = Env { _values  :: Map.Map (Var Resolved) (Type Typed)
        , _types   :: Map.Map (Var Resolved) (Kind Typed)
        , _typeVars :: Set.Set (Var Resolved)
        }
  deriving (Eq, Show, Ord)


instance Monoid Env where
  mappend = (<>)
  mempty = Env mempty mempty mempty

instance Semigroup Env where
  Env a b c <> Env a' b' c' = Env (a <> a') (b <> b') (c <> c')

data Constraint p
  = ConUnify SomeReason (Type p) (Type p)
  | ConSubsume SomeReason (Type p) (Type p)
  | ConImplies SomeReason (Type p) [Constraint p] [Constraint p]
  | ConFail (Var p) (Type p) -- for holes. I hate it.

deriving instance (Show (Ann p), Show (Var p), Show (Expr p), Show (Type p))
  => Show (Constraint p)

deriving instance (Eq (Ann p), Eq (Var p), Eq (Expr p), Eq (Type p))
  => Eq (Constraint p)

data TypeError where
  NotEqual :: Pretty (Var p) => Type p -> Type p -> TypeError
  KindsNotEqual :: Pretty (Var p) => Kind p -> Kind p -> TypeError
  Occurs :: Pretty (Var p) => Var p -> Type p -> TypeError

  NotInScope :: Var Resolved -> TypeError
  FoundHole :: Var Typed -> Type Typed -> TypeError

  Impredicative :: Pretty (Var p) => Var p -> Type p -> TypeError
  ImpredicativeApp :: Pretty (Var p) => Type p -> Type p -> TypeError

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
  IllegalTypeApp :: (Pretty (Var p), Pretty (Var p')) => Expr p -> Type p' -> Type p' -> TypeError

instance (Ord (Var p), Substitutable p (Type p)) => Substitutable p (Constraint p) where
  ftv (ConUnify _ a b) = ftv a `Set.union` ftv b
  ftv (ConSubsume _ a b) = ftv a `Set.union` ftv b
  ftv (ConImplies _ t a b) = ftv a `Set.union` ftv b `Set.union` ftv t
  ftv (ConFail _ t) = ftv t

  apply s (ConUnify e a b) = ConUnify e (apply s a) (apply s b)
  apply s (ConSubsume e a b) = ConSubsume e (apply s a) (apply s b)
  apply s (ConImplies e t a b) = ConImplies e (apply s t) (apply s a) (apply s b)
  apply s (ConFail e t) = ConFail e (apply s t)

instance Pretty (Var p) => Pretty (Constraint p) where
  pretty (ConUnify _ a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (ConSubsume _ a b) = pretty a <+> soperator (string "<=") <+> pretty b
  pretty (ConImplies _ t a b) = brackets (pretty t) <+> hsep (punctuate comma (map pretty a))
                            <+> soperator (char '⊃')
                            <#> indent 2 (vsep (punctuate comma (map pretty b)))
  pretty ConFail{} = string "fail"


makeLenses ''Env -- this has to be down here
-- *shakes fist* grr templatehaskell

lookupTy :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var Resolved -> m (Type Typed)
lookupTy x = do
  rs <- view (values . at x)
  case rs of
    Just t -> fmap thd3 (instantiate t) `catchError` \e ->
      throwError (Note (Note e (string "Arising from instancing of variable" <+> pretty x))
                       (pretty x <+> string "has principal type" <+> pretty t))
    Nothing -> throwError (NotInScope x)

lookupTy' :: (MonadError TypeError m, MonadReader Env m, MonadGen Int m) => Var Resolved
          -> m (Map.Map (Var Typed) (Type Typed), Type Typed, Type Typed)
lookupTy' x = do
  rs <- view (values . at x)
  case rs of
    Just t -> instantiate t `catchError` \e ->
      throwError (Note (Note e (string "Arising from instancing of variable" <+> pretty x))
                       (pretty x <+> string "has principal type" <+> pretty t))
    Nothing -> throwError (NotInScope x)

runInfer :: MonadGen Int m
         => Env
         -> ReaderT Env (WriterT [Constraint p] (ExceptT TypeError m)) a
         -> m (Either TypeError (a, [Constraint p]))
runInfer ct ac = runExceptT (runWriterT (runReaderT ac ct))

fresh :: MonadGen Int m => m (Var Resolved)
fresh = do
  x <- gen
  pure (TgName (alpha !! x) x)

freshFrom :: MonadGen Int m => Text -> m (Var Resolved)
freshFrom t = TgName t <$> gen

extend :: MonadReader Env m => (Var Typed, Type Typed) -> m a -> m a
extend (v, t) = local (values . at (unTvName v) .~ Just t)

extendKind :: MonadReader Env m => (Var Typed, Kind Typed) -> m a -> m a
extendKind (v, k) = local (types . at (unTvName v) .~ Just k)

extendMany :: MonadReader Env m => [(Var Typed, Type Typed)] -> m a -> m a
extendMany ((v, t):xs) b = extend (v, t) $ extendMany xs b
extendMany [] b = b

extendManyK :: MonadReader Env m => [(Var Typed, Kind Typed)] -> m a -> m a
extendManyK = flip (foldr extendKind)

alpha :: [Text]
alpha = map T.pack $ [1..] >>= flip replicateM ['a'..'z']

instantiate :: (MonadError TypeError m, MonadGen Int m) => Type Typed -> m (Map.Map (Var Typed) (Type Typed), Type Typed, Type Typed)
instantiate tp@(TyForall vs ty) = do
  f <- traverse (const freshTV) vs
  let map = Map.fromList (zip vs f)
  (map', _, t) <- instantiate (apply map ty)
  pure (map <> map', tp, t)
instantiate ty = pure (mempty, ty, ty)

difference :: Env -> Env -> Env
difference (Env ma mb mc) (Env ma' mb' mc') = Env (ma Map.\\ ma') (mb Map.\\ mb') (mc Set.\\ mc')

freshTV :: MonadGen Int m => m (Type Typed)
freshTV = TyVar . TvName <$> fresh

freshKV :: MonadGen Int m => m (Kind Typed)
freshKV = KiVar . TvName <$> fresh

freeInScope :: Env -> Set.Set (Var Typed)
freeInScope (Env vars _ _) = foldMap ftv vars

instance Pretty TypeError where
  pretty (NotEqual a b) = string "Type error: failed to" <+> align (string "unify" <+> pretty a </> string " with" <+> pretty b)
  pretty (KindsNotEqual a b) = string "Kind error: failed to" <+> align (string "unify" <+> pretty a </> string " with" <+> pretty b)
  pretty (Occurs v t) = string "Occurs check:" <+> string "The type variable" <+> stypeVar (pretty v) </> indent 4 (string "occurs in the type" <+> pretty t)
  pretty (NotInScope e) = string "Variable not in scope:" <+> pretty e
  pretty (ArisingFrom er ex) = pretty (annotation ex) <> colon <+> stypeSkol (string "error")
    <#> indent 2 (pretty er <#> nest 4 (bullet (string "Arising from use of" <+> blameOf ex) </> pretty ex))
  pretty (FoundHole e s) = string "Found typed hole" <+> pretty e <+> "of type" <+> pretty s

  pretty (Note te m) = pretty te <#> bullet (string "Note:") <+> align (pretty m)
  pretty (Suggestion te m) = pretty te <#> bullet (string "Suggestion:") <+> align (pretty m)
  pretty (CanNotInstance rec new) = string "Can not instance hole of record type" <+> align (verbatim rec </> string " to type " <+> verbatim new)
  pretty (Malformed tp) = string "The type" <+> verbatim tp <+> string "is malformed."
  pretty (NoOverlap ta tb)
    | TyExactRows ra <- ta
    , TyRows _ rb <- tb
    =   string "No overlap between exact record" <+> nest 9 (verbatim ta </> string "and polymorphic record" <+> verbatim tb)
    <#> missing ra rb
    | TyExactRows rb <- tb
    , TyRows _ ra <- ta
    =   string "No overlap between polymorphic record" <+> nest 21 (verbatim ta </> string "and exact record" <+> verbatim tb)
    <#> missing ra rb
    | TyExactRows ra <- ta
    , TyExactRows rb <- tb
    =  string "No overlap between exact records" <+> nest 29 (verbatim ta </> string "and" <+> verbatim tb)
    <#> missing ra rb
    | otherwise
    = string "\x1b[1;32minternal compiler error\x1b[0m: NoOverlap" <+> verbatim ta <+> verbatim tb

  pretty (Impredicative v t)
    = vsep [ string "Illegal instantiation of type variable" <+> stypeVar (pretty v)
           , indent 16 (string "with polymorphic type" <+> verbatim t)
           , bullet (string "Note:") <+> string "doing so would constitute" <+> stypeCon (string "impredicative polymorphism")
           ]
  pretty (ImpredicativeApp tf tx)
    = vsep [ string "Illegal use of polymorphic type" <+> verbatim tx
           , indent 2 $ string "as argument to the type function" <+> verbatim tf
           , bullet (string "Note:") <+> string "instantiating a type variable"
           <+> nest 2 (parens (string "the argument to" <+> verbatim tf)
                   </> string "with a polymorphic type constitutes" <+> stypeCon (string "impredicative polymorphism"))
           ]

  pretty (IllegalTypeApp ex ta _)
    = vsep [ string "Illegal type application" <+> verbatim ex
           , bullet (string "because of type ") <+> verbatim ta
           ]
  pretty (EscapedSkolems esc t) =
    vsep [ case esc of
            [Skolem{..}] ->
              let skol = stypeVar (pretty _skolVar) in
              string "Rigid type variable" <+> skol <+> string "has escaped its scope of" <+> pretty _skolScope
                  <#> bullet (string "Note: the variable") <+> skol <+> string "was rigidified because"
                        <+> nest 8 (prettyMotive _skolMotive <> comma)
                  <#> indent 8 (string "and is represented by constant" <+> stypeSkol (pretty _skolIdent)) 
            _ -> foldr (<#>) empty (map (pretty . flip EscapedSkolems t . pure) esc)
         , empty -- a line break
         , bullet (string "Note: in type") <+> verbatim t
         ]

  pretty (SkolBinding (Skolem _ v _ m) b) =
    vsep [ string "Can not unify rigid type variable" <+> skol <+> string "with" <+> whatIs b
         , bullet (string "Note: the variable") <+> skol <+> string "was rigidified because" <+> prettyMotive m
         , case b of
             TySkol (Skolem _ v _ m) ->
               vsep [ bullet (string "Note: the rigid type variable") <+> stypeVar (pretty v) <> comma <+> string "in turn" <> comma
                    , indent 8 (string "was rigidified because") <+> prettyMotive m
                    ]
             _ -> empty
         ]
    where whatIs (TySkol (Skolem _ v _ _)) = string "the rigid type variable" <+> stypeVar (pretty v)
          whatIs t = string "the type" <+> pretty t
          skol = stypeVar (pretty v)

missing :: [(Text, b)] -> [(Text, b)] -> Doc
missing ra rb
  | length ra < length rb
  =  bullet (string "Namely, the following fields are missing:") <+> hsep (punctuate comma (diff rb ra))
  | length ra > length rb
  =  bullet (string "Namely, the following fields should not be present:") <+> hsep (punctuate comma (diff ra rb))
  | length ra == length rb
  = vsep $ [ bullet (string "Note: no fields match")
           , bullet (string "The following fields are missing:")
            <+> hsep (punctuate comma (diff ra rb))]
missing _ _ = undefined -- freaking GHC

diff :: [(Text, b)] -> [(Text, b)] -> [Doc]
diff ra rb = map (stypeVar . string . T.unpack . fst) (deleteFirstsBy ((==) `on` fst) rb ra)

prettyMotive :: SkolemMotive Typed -> Doc
prettyMotive ByAscription = string "of a type ascription"
prettyMotive (BySubsumption t1 t2) = string "of a subsumption constraint relating" <+> pretty t1 <+> string "with" <+> pretty t2
prettyMotive (ByExistential v t) = string "it is an existential" <> comma <#> string "bound by the type of" <+> pretty v <> comma <+> pretty t
