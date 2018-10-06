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
  , SomeReason(..), Reasonable, addBlame
  , becauseExp, becausePat
  , WhyInstantiate(..)
  , WhyUnsat(..)

  -- lenses:
  , names, typeVars
  )
  where

import Control.Monad.Writer.Strict as M hiding ((<>))
import Control.Monad.Chronicles as M
import Control.Monad.Reader as M
import Control.Monad.Namey as M
import Control.Lens

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Function
import Data.Typeable
import Data.Foldable
import Data.Spanned
import Data.Triple
import Data.Reason
import Data.Maybe
import Data.These
import Data.Text (Text)
import Data.List
import Data.Span

import Text.Pretty.Semantic
import Text.Pretty.Note

import Syntax.Transform
import Syntax.Implicits
import Syntax.Pretty
import Syntax.Types
import Syntax.Subst

type MonadInfer p m =
  ( MonadChronicles TypeError m
  , MonadReader Env m
  , MonadWriter (Seq.Seq (Constraint p)) m
  , MonadNamey m)

data Constraint p
  = ConUnify    SomeReason (Var p) (Type p) (Type p)
  | ConImplies  SomeReason (Type p) (Seq.Seq (Constraint p)) (Seq.Seq (Constraint p))
  | ConSubsume  SomeReason (ImplicitScope p) (Var p) (Type p) (Type p)
  | ConImplicit SomeReason (ImplicitScope p) (Var p) (Type p)
  | ConFail (Ann p) (Var p) (Type p) -- for holes. I hate it.
  | DeferredError TypeError

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
  AmbiguousType :: (Ord (Var p), Pretty (Var p)) => Var p -> Type p -> Set.Set (Var p) -> TypeError
  PatternRecursive :: Binding Resolved -> [Binding Resolved] -> TypeError

  DeadBranch :: TypeError -> TypeError

  UnsatClassCon :: SomeReason -> Constraint Typed -> WhyUnsat -> TypeError

  NotPromotable :: Pretty (Var p) => Var p -> Type p -> Doc -> TypeError

data WhyInstantiate = Expression | Subsumption
data WhyUnsat = NotAFun | PatBinding

instance (Show (Ann p), Show (Var p), Ord (Var p), Substitutable p (Type p)) => Substitutable p (Constraint p) where
  ftv (ConUnify _ _ a b) = ftv a <> ftv b
  ftv (ConSubsume _ s _ a b) = foldMap ftv (keys s) <> ftv a <> ftv b
  ftv (ConImplicit _ s _ b) = foldMap ftv (keys s) <> ftv b
  ftv (ConImplies _ t a b) = ftv a <> ftv b <> ftv t
  ftv (ConFail _ _ t) = ftv t
  ftv DeferredError{} = mempty

  apply s (ConUnify e v a b) = ConUnify e v (apply s a) (apply s b)
  apply s (ConSubsume e c v a b) = ConSubsume e (mapTypes (apply s) c) v (apply s a) (apply s b)
  apply s (ConImplies e t a b) = ConImplies e (apply s t) (apply s a) (apply s b)
  apply s (ConImplicit r c v t) = ConImplicit r (mapTypes (apply s) c) v (apply s t)
  apply s (ConFail a e t) = ConFail a e (apply s t)
  apply _ x@DeferredError{} = x

instance Pretty (Var p) => Pretty (Constraint p) where
  pretty (ConUnify _ _ a b) = pretty a <+> soperator (char '~') <+> pretty b
  pretty (ConSubsume _ _ _ a b) = pretty a <+> soperator (string "<:") <+> pretty b
  pretty (ConImplies _ t a b) = brackets (pretty t) <+> hsep (punctuate comma (toList (fmap pretty a)))
                            <+> soperator (char '⊃')
                            <#> indent 2 (vsep (punctuate comma (toList (fmap pretty b))))
  pretty (ConImplicit _ _ v t) = pretty v <+> colon <+> pretty t
  pretty ConFail{} = string "fail"
  pretty DeferredError{} = string "deferred type error"

instance Show TypeError where
  show _ = "use pretty for displaying type errors"

instance Eq TypeError where
  _ == _ = False

lookupTy :: (MonadChronicles TypeError m, MonadReader Env m, MonadNamey m) => Var Resolved -> m (Type Typed)
lookupTy x = do
  rs <- view (names . at x)
  case rs of
    Just t -> thd3 <$> instantiate Expression t
    Nothing -> confesses (NotInScope x)

lookupTy' :: (MonadChronicles TypeError m, MonadReader Env m, MonadNamey m) => Var Resolved
          -> m (Maybe (Expr Typed -> Expr Typed), Type Typed, Type Typed)
lookupTy' x = do
  rs <- view (names . at x)
  case rs of
    Just t -> instantiate Expression t
    Nothing -> confesses (NotInScope x)

runInfer :: MonadNamey m
         => Env
         -> ReaderT Env (WriterT (Seq.Seq (Constraint p)) (ChroniclesT TypeError m)) a
         -> m (These [TypeError] (a, Seq.Seq (Constraint p)))
runInfer ct ac = over here toList <$>
  runChronicleT (runWriterT (runReaderT ac ct))

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

  pretty (NotInScope e) = string "Variable not in scope:" <+> pretty e
  pretty (ArisingFrom er ex) = pretty er <#> empty <#> nest 4 (string "Arising in" <+> blameOf ex)
  pretty (FoundHole e s) = string "Found typed hole" <+> pretty e <+> "of type" <+> displayType s

  pretty (Note te m) = pretty te <#> note <+> align (pretty m)
  pretty (Suggestion te m) = pretty te <#> bullet (string "Suggestion:") <+> align (pretty m)
  pretty (CanNotInstance rec new) = string "Can not instance hole of record type" <+> align (verbatim rec </> string " to type " <+> verbatim new)
  pretty (Malformed tp) = string "The type" <+> verbatim tp <+> string "is malformed."

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

  pretty (Occurs v t) =
    vsep [ "The type variable" <+> stypeVar (squote <> pretty v) <+> "appears in the type" <+> displayType t
         , empty
         , bullet "Note: all solutions to"
         , indent 4 (stypeVar (squote <> pretty v) <+> soperator (string "~") <+> displayType t)
         , indent 2 "are infinite."
         ]

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
         , note <+> string "in type" <+> displayType t
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
             ByAscription _ t ->
               vsep [ bullet "When checking that this expression has type"
                    , indent 5 (displayType t) ]
             BySubsumption s t ->
               vsep [ bullet $ string "When checking that the type"
                    , indent 5 (displayType s)
                    , indent 2 (string "can be made as polymorphic as")
                    , indent 5 (displayType t) ]
             ByExistential c t ->
               vsep [ bullet $ string "Where the type variable" <+> stypeSkol (pretty v) <+> "is an" <+> keyword "existential" <> comma
                    , indent 2 $ string "bound by the constructor" <+> stypeCon (pretty c) <> ", which has type"
                    , indent 5 (displayType t)
                    ]
           ]
   where whatIs (TySkol (Skolem _ v _ _)) = string "the rigid type variable" <+> stypeVar (squote <>pretty v)
         whatIs t = string "the type" <+> displayType (withoutSkol t)

  pretty (AmbiguousType v t (Set.toList -> vs)) =
    vsep [ "Ambiguous type for value:" <+> stypeSkol (pretty v)
         , empty
         , indent 2 $ displayType t
         , empty
         , bullet "Note:" <+> vars <+> "appears in a constraint,"
         , indent 4 "but not in the consequent of the type"
         ]
    where
      vars = case vs of
        [x] -> "The variable" <+> stypeSkol (pretty x)
        xs -> "The variables"
                <+> hsep (punctuate comma (map (stypeSkol . pretty) xs))

  pretty (PatternRecursive _ _) = string "pattern recursive error should be formatNoted"
  pretty DeadBranch{} = string "dead branch error should be formatNoted"
  pretty UnsatClassCon{} = string "unsat class error should be formatNoted"

instance Spanned TypeError where
  annotation (ArisingFrom e@ArisingFrom{} _) = annotation e
  annotation (ArisingFrom _ x) = annotation x
  annotation x = error (show (pretty x))

instance Note TypeError Style where
  diagnosticKind (ArisingFrom e _) = diagnosticKind e
  diagnosticKind DeadBranch{} = WarningMessage
  diagnosticKind _ = ErrorMessage

  formatNote f (ArisingFrom e@ArisingFrom{} _) = formatNote f e
  -- This one gets ~Special Handling~™
  formatNote f (ArisingFrom (SkolBinding (Skolem _ v _ m) t) rs) =
    vsep [ indent 2 "Could not match the rigid type variable" <+> sk (squote <> pretty v) <+> "with" <+> whatIs t
         , empty
         , case m of
             ByAscription ex t ->
               let k =
                     if annotation rs `includes` annotation ex
                        then id
                        else (<#>) (vsep [ indent 2 $ bullet "Arising in" <+> (Right <$> blameOf rs)
                                         , f [annotation rs]
                                         , empty ])
                 in
                  k (vsep [ indent 2 $ bullet "When checking that this expression has type"
                          , indent 5 (Right <$> displayType t)
                          , nest (-2) $ f [annotation ex] ])
             BySubsumption s t ->
               vsep [ indent 2 $ bullet "When checking that the type"
                    , indent 5 (Right <$> displayType s)
                    , indent 4 "can be made as polymorphic as"
                    , indent 5 (Right <$> displayType t)

                    , empty

                    , indent 2 $ bullet "Arising in the" <+> (Right <$> blameOf rs)
                    , f [annotation rs]
                    ]
             ByExistential c t ->
               vsep [ indent 2 $ string "Where the type variable" <+> sk (pretty v) <+> "is an" <+> sk "existential" <> comma
                    , indent 2 $ string "bound by the constructor" <+> sc (pretty c) <> ", which has type"
                    , indent 5 (Right <$> displayType t)

                    , empty

                    , indent 2 $ bullet "Arising in the" <+> (Right <$> blameOf rs)
                    , nest (-2) $ f [annotation rs]
                    ]
           ]
   where whatIs (TySkol (Skolem _ v _ _)) = string "the type" <+> sv (squote <> pretty v)
         whatIs t = string "the type" <+> (Right <$> displayType (withoutSkol t))
         sv = fmap Right . stypeVar
         sk = fmap Right . stypeSkol
         sc = fmap Right . stypeCon

  formatNote f (ArisingFrom (PatternRecursive p [p']) _) | p == p' =
    vsep [ indent 2 "Recursive pattern bindings are not allowed"
         , indent 2 $ bullet "Note: this definition refers to itself" <+> (Right <$> highlight "directly")
         , empty
         , f [annotation p]
         ]

  formatNote f (ArisingFrom (PatternRecursive p bs) _) | bs <- delete p bs =
    vsep [ indent 2 "Pattern bindings can not participate in recursion"
         , empty
         , f [annotation p]
         , empty
         , indent 2 $ bullet "Note: this binding is in the same" <+> (Right <$> highlight "recursive group") <+> string "as these"
           <#> if length bs > 3
                  then vsep [ indent 4 "and" <+> int (length bs - 3) <+> "other binding" <> (if length bs - 3 /= 1 then "s." else ".")
                            , empty ]
                  else empty
         , f (map annotation (take 3 bs))
         ]

  formatNote f (ArisingFrom (DeadBranch e) r) =
    formatNote f (ArisingFrom e r) <#>
      vsep [ indent 2 $ bullet "Note: This branch will never be executed,"
           , indent 4 "because it has unsatisfiable constraints" ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit r _ _ t) NotAFun) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising from use of the expression"
         , f [annotation r]
         , indent 2 $ bullet "Note: this constraint was not quantified over"
         , indent 4 "because the binding it would scope over is not a function"
         , f [annotation r']
         , indent 2 $ bullet "Possible fix: add a parameter, or a type signature"
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) PatBinding) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising in the binding"
         , f [annotation r']
         , indent 2 $ bullet "Note: this constraint can not be quantified over"
         , indent 4 "because it is impossible to quantify over pattern bindings"
         ]

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

addBlame :: SomeReason -> TypeError -> TypeError
addBlame _ e@ArisingFrom{} = e
addBlame x e = ArisingFrom e x

withoutSkol :: Type p -> Type p
withoutSkol = transformType go where
  go (TySkol x) = TyVar (x ^. skolVar)
  go x = x
