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
  , NamedFieldPuns
  #-}
module Control.Monad.Infer
  ( module M, firstName
  , TypeError(..)
  , Constraint(..)
  , Env
  , MonadInfer, Name
  , lookupTy, lookupTy', genNameFrom, genNameWith, runInfer, freeInEnv
  , difference, freshTV, refreshTV
  , instantiate
  , SomeReason(..), Reasonable, addBlame
  , becauseExp, becausePat
  , WhyInstantiate(..)
  , WhyUnsat(..)

  -- lenses:
  , names, typeVars
  , InstLevel(..)
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
import Data.These.Lens
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
import Syntax.Boolean
import Syntax.Builtin
import Syntax.Types
import Syntax.Subst
import Syntax

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
  NotEqual :: (Show (Var p), Pretty (Var p), Ord (Var p))
           => { actual :: Type p
              , expected :: Type p }
           -> TypeError

  Occurs :: (Pretty (Var p), Ord (Var p)) => Var p -> Type p -> TypeError

  NotInScope :: Var Desugared -> TypeError
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

  PatternRecursive :: Binding Desugared -> [Binding Desugared] -> TypeError
  DeadBranch :: TypeError -> TypeError

  AmbiguousType :: (Ord (Var p), Pretty (Var p)) => Var p -> Type p -> Set.Set (Var p) -> TypeError
  AmbiguousMethodTy :: (Ord (Var p), Pretty (Var p)) => Var p -> Type p -> Set.Set (Var p) -> TypeError

  UnsatClassCon :: SomeReason -> Constraint Typed -> WhyUnsat -> TypeError
  Overlap :: Type Typed -> Span -> Span -> TypeError
  ClassStackOverflow :: SomeReason -> [Type Typed] -> Type Typed -> TypeError
  WrongClass :: Binding Desugared -> Var Typed -> TypeError
  UndefinedMethods :: Type Typed -> Formula Text -> Span -> TypeError
  InvalidContext :: String -> Span -> Type Desugared -> TypeError

  CanNotVta :: Type Typed -> Type Desugared -> TypeError

  NotPromotable :: Pretty (Var p) => Var p -> Type p -> Doc -> TypeError
  WildcardNotAllowed :: SomeReason -> TypeError

  NotValue :: SomeReason -> Type Typed -> TypeError
  UnsaturatedTS :: SomeReason -> TySymInfo -> Int -> TypeError

data WhyInstantiate = Expression | Subsumption
data WhyUnsat
  = NotAFun
  | PatBinding
  | RecursiveDeduced 
  | InstanceMethod (Type Typed)
  | InstanceClassCon Span
  | BadDefault (Var Desugared) (Type Typed)
  | GivenContextNotEnough (Type Typed)
  | TooConcrete (Type Typed)
  | It'sQuantified

instance (Show (Ann p), Show (Var p), Ord (Var p), Substitutable p (Type p))
          => Substitutable p (Constraint p) where
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
  pretty (ConUnify _ v a b) = pretty v <+> colon <+> pretty a <+> soperator (char '~') <+> pretty b
  pretty (ConSubsume _ _ v a b) = pretty v <+> colon <+> pretty a <+> soperator (string "<:") <+> pretty b
  pretty (ConImplies _ t a b) = brackets (pretty t) <+> hsep (punctuate comma (toList (fmap pretty a)))
                            <+> soperator (char '⊃')
                            <#> indent 2 (vsep (punctuate comma (toList (fmap pretty b))))
  pretty (ConImplicit _ _ v t) = pretty v <+> colon <+> pretty t
  pretty ConFail{} = string "fail"
  pretty DeferredError{} = string "deferred type error"

instance Show TypeError where
  show = show . pretty

instance Eq TypeError where
  _ == _ = False

data InstLevel = Weak | Strong
  deriving (Eq)

lookupTy :: (MonadChronicles TypeError m, MonadReader Env m, MonadNamey m) => Var Desugared -> m (Type Typed)
lookupTy x = do
  rs <- view (names . at x)
  case rs of
    Just t -> thd3 <$> instantiate Strong Expression t
    Nothing -> confesses (NotInScope x)

lookupTy' :: (MonadChronicles TypeError m, MonadReader Env m, MonadNamey m)
          => InstLevel -> Var Desugared
          -> m (Maybe (Expr Typed -> Expr Typed), Type Typed, Type Typed)
lookupTy' str x = do
  rs <- view (names . at x)
  case rs of
    Just t -> instantiate str Expression t
    Nothing -> confesses (NotInScope x)

runInfer :: MonadNamey m
         => Env
         -> ReaderT Env (WriterT (Seq.Seq (Constraint p)) (ChroniclesT TypeError m)) a
         -> m (These [TypeError] (a, Seq.Seq (Constraint p)))
runInfer ct ac = over here toList <$>
  runChronicleT (runWriterT (runReaderT ac ct))

genNameFrom :: MonadNamey m => Text -> m (Var Desugared)
genNameFrom t = do
  ~(TgName _ n) <- genName
  pure (TgName t n)

genNameWith :: MonadNamey m => Text -> m (Var Desugared)
genNameWith t = do
  ~(TgName e n) <- genName
  pure (TgName (t <> e) n)

firstName :: Var Desugared
firstName = TgName "a" 0

instantiate :: MonadNamey m
            => InstLevel
            -> WhyInstantiate
            -> Type Typed
            -> m ( Maybe (Expr Typed -> Expr Typed)
                 , Type Typed
                 , Type Typed)
instantiate str r tp@(TyPi (Invisible v _ spec) ty) | can str spec = do
  var <- refreshTV v
  let map = Map.singleton v var

      appThisTy e = ExprWrapper (TypeApp var) e (annotation e, apply map ty)
  (k, _, t) <- instantiate str r (apply map ty)
  pure (squish appThisTy k, tp, t)

instantiate str r tp@(TyPi (Anon co) od@dm) = do
  (wrap, _, dm) <- instantiate str r dm
  let cont = fromMaybe id wrap
  var <- genName
  let ty = TyPi (Anon co) dm
      lam :: Expr Typed -> Expr Typed
      lam e | od == dm = e
      lam e
        | ann <- annotation e
        = Fun (PatParam (PType (Capture var (ann, co)) co (ann, co)))
           (cont (App e (VarRef var (ann, co)) (ann, od))) (ann, ty)

  pure (Just lam, tp, ty)

instantiate str r tp@(TyPi (Implicit co) od@dm) = do
  (wrap, _, dm) <- instantiate str r dm
  let cont = fromMaybe id wrap
  var <- genName
  let ty = TyPi (Implicit co) dm
      lam :: Expr Typed -> Expr Typed
      lam e | od == dm = e
      lam e
        | ann <- annotation e
        = Fun (EvParam (PType (Capture var (ann, co)) co (ann, co)))
            (cont (App e (VarRef var (ann, co)) (ann, od))) (ann, ty)

  pure (Just lam, tp, ty)

instantiate _ _ ty = pure (Just id, ty, ty)

can :: InstLevel -> Specificity -> Bool
can Strong x = case x of
  Req -> False
  _ -> True
can Weak Infer = True
can Weak _ = False

freshTV :: MonadNamey m => m (Type Typed)
freshTV = TyVar <$> genName

refreshTV :: MonadNamey m => Var Typed -> m (Type Typed)
refreshTV v = TyVar <$> genNameFrom nm where
  nm = case v of
    TgInternal x -> x
    TgName x _ -> x

instance Pretty TypeError where
  pretty NotEqual{ expected, actual } =
    vsep
      $ nest 2 ("Couldn't match" <+> highlight "actual" <+> "type" <+> displayType actual
           </> "with the" <+> highlight "type expected by the context," <+> displayType expected)
      : case (expected, actual) of
          (TyArr{}, TyArr{}) -> []
          (TyArr{}, t) ->
            [ empty, bullet "Did you apply a" <+> describe t <+> "to too many arguments?" ]
          (t, TyArr{}) ->
            [ empty, bullet "Did you forget some of the arguments to a" <+> describe t <> "?" ]
          (_, _) -> []
    where
      describe TyType = "type constructor"
      describe _ = "function"

  pretty (NotInScope e) = string "Variable not in scope:" <+> pretty e
  pretty (ArisingFrom er _) = pretty er
  pretty (FoundHole e s) = string "Found typed hole" <+> pretty e <+> "of type" <+> displayType s

  pretty (Note te m) = pretty te <#> note <+> pretty m
  pretty (Suggestion te m) = pretty te <#> bullet (string "Suggestion:") <+> align (pretty m)
  pretty (CanNotInstance rec new) =
    string "Can not instance hole of record type"
    <+> align (verbatim rec </> string " to type " <+> verbatim new)
  pretty (Malformed tp) = string "The type" <+> verbatim tp <+> string "is malformed."

  pretty (NoOverlap ta tb)
    | TyExactRows ra <- ta
    , TyRows _ rb <- tb
    = string "No overlap between exact record"
      <+> nest 9 (displayType ta </> string "and polymorphic record" <+> displayType tb)
    <#> missing ra rb
    | TyExactRows rb <- tb
    , TyRows _ ra <- ta
    = string "No overlap between polymorphic record"
      <+> nest 21 (displayType ta </> string "and exact record" <+> displayType tb)
    <#> missing ra rb
    | TyExactRows ra <- ta
    , TyExactRows rb <- tb
    = string "No overlap between exact records"
    <+> nest 29 (displayType ta </> string "and" <+> displayType tb)
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
                   </> string "with a polymorphic type constitutes"
                   <+> stypeCon (string "impredicative polymorphism"))
           ]

  pretty (EscapedSkolems [skol] _) | ByConstraint con <- _skolMotive skol =
    vsep [ "The constraint" <+> displayType con <+> "is ambiguous" ]

  pretty (EscapedSkolems esc t) =
    vsep [ case esc of
            [Skolem{..}] ->
              let skol = stypeVar (pretty _skolVar) in
              string "Rigid type variable" <+> skol
                <+> string "has escaped its scope of" <+> displayType _skolScope
                <#> rest skol _skolMotive
            _ -> foldr ((<#>) . pretty . flip EscapedSkolems t . pure) empty esc
         ]
    where rest skol x =
            case x of
              ByConstraint t ->
                vsep [ note <+> "This variable was rigidified because of an ambiguous constraint:"
                     , indent 4 $ displayType t ]
              _ -> note <+> string "the variable" <+>
                   skol <+> string "was rigidified because" <+> nest 8 (prettyMotive x <> comma)

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
               vsep [ bullet $ string "Where the type variable"
                        <+> stypeSkol (pretty v) <+> "is an" <+> keyword "existential" <> comma
                    , indent 2 $ string "bound by the constructor"
                        <+> stypeCon (pretty c) <> ", which has type"
                    , indent 5 (displayType t)
                    ]
             ByInstanceHead h _ ->
               vsep [ bullet "Where the type variable"
                        <+> stypeSkol (pretty v) <+> "is bound by the instance head"
                    , indent 5 (displayType h)
                    ]
             ByConstraint{} -> error "Impossible"
           ]
   where whatIs (TySkol (Skolem _ v _ _)) = string "the rigid type variable" <+> stypeVar (squote <>pretty v)
         whatIs t = string "the type" <+> displayType (withoutSkol t)

  pretty (AmbiguousType v t (Set.toList -> vs)) =
    vsep [ "Ambiguous type for value" <+> stypeSkol (pretty v)
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

  pretty (AmbiguousMethodTy v _ (Set.toList -> vs)) =
    vsep [ "Ambiguous type for method" <+> stypeSkol (pretty v)
         , bullet "Note:" <+> vars <+> "bound in the class head"
         , "but" <+> appear <+> "in the method type"
         ]
    where
      vars = case vs of
        [x] -> "The variable" <+> stypeSkol (pretty x) <+> "is"
        xs -> "The variables"
                <+> hsep (punctuate comma (map (stypeSkol . pretty) xs))
                <+> "are"
      appear = case vs of
        [_] -> "doesn't appear"
        _ -> "don't appear"

  pretty (ClassStackOverflow _ xs t) =
    vsep [ "Stack overflow while looking for an instance of"
         , indent 2 $ displayType t
         , bullet "Note: the max depth of typeclass constraints is 25."
         , bullet "Here are the first five entries on the stack:"
         , vsep (map (indent 2 . bullet . displayType) (take 5 (reverse xs)))
         ]

  pretty (WrongClass (Binding v _ _ _) c) =
    vsep [ "Method" <+> pretty v <+> "is not a member of the class" <+> stypeCon (pretty c) ]
  pretty (WrongClass _ _) = error "Impossible"

  pretty (UndefinedMethods h xs _) =
    vsep [ "Missing implementation of methods in instance for" <+> displayType h
         , "Namely, there must be an implementation for at least"
         , indent 2 (align (pretty (fmap TgInternal xs)))
         ]

  pretty (InvalidContext what _ ty) =
    vsep [ "Invalid type in context for" <+> string what <+> "declaration:"
         , indent 4 (displayType ty)
         ]

  pretty (CanNotVta ty arg) =
    vsep [ "Can not apply expression of type" <+> displayType ty
         , indent 2 "to visible type argument" <+> displayType arg
         ]

  pretty (NotValue _ t) =
    vsep [ "Since evaluating this expression may have side-effects"
         , "it can not be used in a context where the polymorphic type"
         , indent 2 (displayType t)
         , "is required."
         ]

  pretty (UnsaturatedTS _ info n) =
    vsep [ "Type synonym" <+> stypeCon (pretty (info ^. tsName)) <+> "appears with" <+> nargs <> comma
         , "but in its definition it has" <+> sliteral (int (length (info ^. tsArgs)))
         ]
    where
      nargs =
        case n of
          0 -> "no arguments"
          1 -> "one argument"
          x -> sliteral (int x) <+> "arguments"


  pretty WildcardNotAllowed{} = "Type wildcard not allowed here"

  pretty (PatternRecursive _ _) = string "pattern recursive error should be formatNoted"
  pretty (DeadBranch e) = string "dead branch error should be formatNoted" <+> pretty e
  pretty (UnsatClassCon _ t _) = string "unsatClassCon" <+> pretty t
  pretty Overlap{} = string "overlap error should be formatNoted"


instance Spanned TypeError where
  annotation (ArisingFrom e@ArisingFrom{} _) = annotation e
  annotation (ArisingFrom _ x) = annotation x
  annotation (Overlap _ x _) = annotation x
  annotation (ClassStackOverflow x _ _) = annotation x
  annotation (WrongClass x _) = annotation x
  annotation (UndefinedMethods _ _ x) = annotation x
  annotation (InvalidContext _ x _) = annotation x
  annotation (WildcardNotAllowed x) = annotation x
  annotation (NotValue x _) = annotation x
  annotation (UnsaturatedTS x _ _) = annotation x
  annotation x = error (show (pretty x))

instance Note TypeError Style where
  diagnosticKind (ArisingFrom e _) = diagnosticKind e
  diagnosticKind DeadBranch{} = WarningMessage
  diagnosticKind _ = ErrorMessage

  formatNote f (ArisingFrom e@ArisingFrom{} _) = formatNote f e
  -- This one gets ~Special Handling~™
  formatNote f (ArisingFrom (SkolBinding (Skolem _ v _ m) t) rs) =
    vsep [ indent 2 "Could not match the rigid type variable"
            <+> sk (squote <> pretty v) <+> "with" <+> whatIs t
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
               vsep [ indent 2 $ string "Where the type variable"
                        <+> sk (pretty v) <+> "is an" <+> sk "existential" <> comma
                    , indent 2 $ string "bound by the constructor" <+> sc (pretty c) <> ", which has type"
                    , indent 5 (Right <$> displayType t)

                    , empty

                    , indent 2 $ bullet "Arising in the" <+> (Right <$> blameOf rs)
                    , nest (-2) $ f [annotation rs]
                    ]
             ByInstanceHead v t ->
               vsep [ indent 2 "Where the type variable" <+> sk (pretty v)
                        <+> "is bound by the instance being defined"
                    , f [t]
                    , empty
                    , indent 2 $ bullet "Arising in the" <+> (Right <$> blameOf rs)
                    , nest (-2) $ f [annotation rs]
                    ]
             ByConstraint{} -> error "Impossible"
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
         , indent 2 $ bullet "Note: this binding is in the same"
           <+> (Right <$> highlight "recursive group") <+> string "as these"
           <#> if length bs > 3
                  then vsep [ indent 4 "and" <+> int (length bs - 3)
                              <+> "other binding" <> (if length bs - 3 /= 1 then "s." else ".")
                            , empty ]
                  else empty
         , f (map annotation (take 3 bs))
         ]

  formatNote f (ArisingFrom (DeadBranch e) r) =
    vsep [ indent 2 "Note: This branch will never be executed,"
         , indent 2 "because it has unsatisfiable constraints" ]
     <#> formatNote f (ArisingFrom e r)

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit r _ _ t) RecursiveDeduced) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising from use of the expression"
         , f [annotation r]
         , mempty
         , indent 2 $ bullet "Note: this constraint was not quantified over because"
         , indent 4 "recursive binding groups must have complete type signatures"
         , indent 4 "in all of their bindings."
         , mempty
         , f [annotation r']
         , indent 3 "This binding should have had a complete type signature."
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit r _ _ t) (TooConcrete _)) _) =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising from use of the expression"
         , f [annotation r]
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit r _ _ t) NotAFun) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising from use of the expression"
         , f [annotation r]
         , indent 2 $ bullet "Note: this constraint was not quantified over because"
         , indent 4 "the binding it would scope over is not a function"
         , if annotation r /= annotation r' then f [annotation r'] else empty
         , indent 2 $ bullet "Possible fix: add a parameter, or a type signature"
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) PatBinding) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising in the binding"
         , f [annotation r']
         , indent 2 $ bullet "Note: this constraint can not be quantified over"
         , indent 4 "because it is impossible to quantify over pattern bindings"
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) (InstanceMethod ctx)) r') =
    vsep [ nest 4 $
            indent 2 "Could not deduce" <+> (Right <$> displayType t)
              <+> "from the context" </> (Right <$> displayType ctx)
         , empty
         , indent 2 $ bullet "Note: this constraint can not be quantified over"
         , indent 4 "because it originated from a method in an instance."
         , empty
         , indent 2 $ bullet "Possible fix: add it to the instance context"
         , empty
         , indent 2 "Arising in the" <+> (Right <$> blameOf r')
         , f [annotation r']
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit why _ v tau) (GivenContextNotEnough ctx)) _) =
    vsep [ f [annotation why]
         , msg
         , indent 2 $ bullet "Possible fix: add it to the type signature for" <+> (Right <$> pretty v)
         , empty
         ]
    where
      msg | TyCon v <- ctx, v == tyUnitName =
              indent 2 "No instance for" <+> (Right <$> displayType tau)
                <+> "arising from a use of" <+> (Right <$> blameOf why)
          | otherwise =
              indent 2 "Could not deduce" <+> (Right <$> displayType tau)
                <+> "from the context" <+> (Right <$> displayType ctx)


  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) (BadDefault meth ty)) r') =
    vsep [ nest 2 $
            indent 2 "No instance for" <+> (Right <$> displayType t)
              <+> "when checking that" <+> (Right <$> stypeSkol (pretty meth))
              </> "is an implementation for type"
         , indent 4 (Right <$> displayType ty)

         , empty

         , indent 2 $ bullet "Note: default method implementations should always be applicable,"
         , indent 4 "and thus can not have any extra constraints"
         , empty

         , indent 2 "Arising in the" <+> (Right <$> blameOf r')
         , f [annotation r']
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) (InstanceClassCon ann)) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising in the instance declaration"
         , f [annotation r']
         , indent 2 $ bullet "Note: this is required by the context of the class,"
         , f [ann]
         ]

  formatNote f (ArisingFrom (UnsatClassCon _ (ConImplicit _ _ _ t) It'sQuantified) r') =
    vsep [ indent 2 "No instance for" <+> (Right <$> displayType t) <+> "arising in" <+> (Right <$> blameOf r')
         , f [annotation r']
         , indent 2 $ bullet "Note: This constraint can not be quantified over because it is of higher rank"
         ]

  formatNote f err@(UnsatClassCon r' _ _) = formatNote f (ArisingFrom err r')

  formatNote f (Overlap tau one two) =
    vsep [ indent 2 "Overlapping instances for" <+> (Right <$> displayType tau)
         , indent 2 $ bullet "Note: first defined here,"
         , f [one]
         , indent 3 "but also defined here"
         , f [two]
         ]

  formatNote f x = f [annotation x] <#> indent 2 (Right <$> pretty x)

missing :: [(Text, b)] -> [(Text, b)] -> Doc
missing ra rb
  | length ra < length rb
  = bullet (string "Namely, the following fields are missing:") <+> hsep (punctuate comma (diff rb ra))
  | length ra > length rb
  = bullet (string "Namely, the following fields should not be present:")
  <+> hsep (punctuate comma (diff ra rb))
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
