{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances,
   StandaloneDeriving, TypeFamilies, DataKinds, DeriveDataTypeable,
   TemplateHaskell, PatternSynonyms #-}
module Syntax where

import qualified Data.Text as T
import Data.List.NonEmpty(NonEmpty ((:|)))
import Data.Semigroup (sconcat)
import Data.Text (Text)
import Data.Typeable
import Data.Spanned
import Data.Maybe
import Data.Span
import Data.Data

import Syntax.Var

import Control.Lens hiding (Lazy)

type family Ann a :: * where
  Ann Parsed = Span
  Ann Resolved = Span
  Ann Desugared = Span
  Ann Typed = (Span, Type Typed)

data Binding p
  -- | @let implicit f x = ...@
  = Binding { _bindVariable :: Var p
            , _bindBody :: Expr p
            , _bindVerify :: Bool
            , _bindAnn :: Ann p
            }
  -- | @let (a, b) = ...@
  | Matching { _bindPattern :: Pattern p
             , _bindBody :: Expr p
             , _bindAnn :: Ann p
             }

  -- | @let (a, b) = ...@
  | TypedMatching { _bindPattern :: Pattern p -- fucking ghc, p ~ Typed
                  , _bindBody :: Expr p -- fucking ghc, p ~ Typed
                  , _bindAnn :: Ann p -- fucking ghc, p ~ Typed
                  , _bindBindings :: [(Var Typed, Type Typed)]
                  }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Binding p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Binding p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Binding p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Binding p)

data Parameter p
  = PatParam { _paramPat :: Pattern p }
  | EvParam { _paramPat :: Pattern p }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Parameter p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Parameter p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Parameter p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Parameter p)

data Expr p
  = VarRef (Var p) (Ann p)
  | Let [Binding p] (Expr p) (Ann p)
  | If (Expr p) (Expr p) (Expr p) (Ann p)
  | App (Expr p) (Expr p) (Ann p)
  | Fun (Parameter p) (Expr p) (Ann p)
  | Begin [Expr p] (Ann p)
  | Literal Lit (Ann p)
  | Match (Expr p) [Arm p] (Ann p)
  | Function [Arm p] (Ann p)
  | BinOp (Expr p) (Expr p) (Expr p) (Ann p)
  | Hole (Var p) (Ann p)
  | Ascription (Expr p) (Type p) (Ann p)

  -- Records
  | Record [Field p] (Ann p) -- { foo = bar, baz = quux }
  | RecordExt (Expr p) [Field p] (Ann p) -- { foo with baz = quux }
  | Access (Expr p) Text (Ann p) -- foo.bar

  -- Sections
  | LeftSection (Expr p) (Expr p) (Ann p) -- (+ foo)
  | RightSection (Expr p) (Expr p) (Ann p) -- (foo +)
  | BothSection (Expr p) (Ann p) -- (+)
  | AccessSection Text (Ann p)
  | Parens (Expr p) (Ann p) -- (xyz), just useful for resetting precedence

  -- Tuple (see note [1])
  | Tuple [Expr p] (Ann p)
  | TupleSection [Maybe (Expr p)] (Ann p)

  -- Module
  | OpenIn (Var p) (Expr p) (Ann p)

  -- Laziness
  | Lazy (Expr p) (Ann p)

  -- Visible type application
  | Vta (Expr p) (Type p) (Ann p)

  -- Lists
  | ListExp [Expr p] (Ann p)
  | ListComp (Expr p) [CompStmt p] (Ann p)

  | ExprWrapper (Wrapper p) (Expr p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Expr p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Expr p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Expr p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Expr p)

data CompStmt p
  = CompGen (Pattern p) (Expr p) (Ann p)
  | CompLet [Binding p] (Ann p)
  | CompGuard (Expr p) 

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (CompStmt p)
deriving instance (Show (Var p), Show (Ann p)) => Show (CompStmt p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (CompStmt p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (CompStmt p)

data Arm p
  = Arm { armPat :: Pattern p
        , armGuard :: Maybe (Expr p)
        , armExp :: Expr p
        }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Arm p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Arm p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Arm p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Arm p)

data Field p =
  Field { _fName :: Text
        , _fExpr :: Expr p
        , _fAnn :: Ann p
        }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Field p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Field p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Field p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Field p)

data Wrapper p
  = TypeApp (Type p)
  | Cast (Coercion p)
  | TypeLam (Skolem p) (Type p)
  | (:>) (Wrapper p) (Wrapper p)
  | TypeAsc (Type p) -- invisible (to pretty-printer) ascription
  | WrapVar (Var p) -- Unsolved wrapper variable
  | ExprApp (Expr p)
  | WrapFn (WrapCont p)
  | IdWrap

data WrapCont p = MkWrapCont { runWrapper :: Expr p -> Expr p, desc :: String }

deriving instance Typeable p => Typeable (WrapCont p)
instance Typeable p => Data (WrapCont p) where
  gunfold _ _ = error "gunfold WrapCont"
  toConstr _ = error "toConstr WrapCont"
  dataTypeOf _ = error "dataTypeOf WrapCont"

instance Eq (WrapCont p) where _ == _ = False
instance Ord (WrapCont p) where _ `compare` _ = GT

instance Show (WrapCont p) where
  show = show . desc

infixr 5 :>

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Wrapper p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Wrapper p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Wrapper p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Wrapper p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Wrapper p)

data Pattern p
  = Wildcard (Ann p)
  | Capture (Var p) (Ann p)
  | Destructure (Var p) (Maybe (Pattern p)) (Ann p)
  | PAs (Pattern p) (Var p) (Ann p)
  | PType (Pattern p) (Type p) (Ann p)
  | PRecord [(Text, Pattern p)] (Ann p)
  | PTuple [Pattern p] (Ann p)
  | PList [Pattern p] (Ann p)
  | PLiteral Lit (Ann p)
  | PWrapper (Wrapper p, Type p) (Pattern p) (Ann p)
  | PSkolem (Pattern p) [Var p] (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Pattern p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Pattern p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Pattern p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Pattern p)

data Lit
  = LiFloat Double
  | LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

data Type p
  = TyCon (Var p)
  | TyVar (Var p)
  | TyPromotedCon (Var p)
  | TyApp (Type p) (Type p)
  | TyPi (TyBinder p) (Type p) -- arrow, pi or forall
  | TyRows (Type p) [(Text, Type p)]  -- { α | foo : int, bar : string }
  | TyExactRows [(Text, Type p)] -- { foo : int, bar : string }
  | TyTuple (Type p) (Type p) -- (see note [1])
  | TyOperator (Type p) (Var p) (Type p)
  | TyWildcard (Maybe (Type p))
  | TyParens (Type p) -- | @(xyz)@, just useful for resetting precedence. Removed after the resolver.

  -- Used internally:
  | TySkol (Skolem p)
  | TyWithConstraints [(Type p, Type p)] (Type p)

  -- Dependent type stuff
  | TyType -- yeah, type : type, fight me

data TyBinder p
  = Anon { _tyBinderType :: Type p } -- a function type
  | Implicit { _tyBinderType :: Type p } -- a type with class obligations
  | Invisible
    { _tyBinderVar :: Var p
    , _tyBinderArg :: Maybe (Type p) } -- a forall. type

deriving instance (Show (Var p), Show (Ann p)) => Show (TyBinder p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TyBinder p)
deriving instance Eq (Var p) => Eq (TyBinder p)
deriving instance Ord (Var p) => Ord (TyBinder p)

data Skolem p
  = Skolem { _skolIdent :: Var p -- the constant itself
           , _skolVar :: Var p -- what variable this skolemises
           , _skolScope :: Type p -- the type this was generated for
           , _skolMotive :: SkolemMotive p
           }

deriving instance (Show (Var p), Show (Ann p)) => Show (Skolem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Skolem p)

data SkolemMotive p
  = ByAscription (Expr Desugared) (Type p) -- what r phases?
  | BySubsumption (Type p) (Type p)
  | ByExistential (Var p) (Type p)
  | ByInstanceHead (Type p) (Ann Desugared)
  | ByConstraint (Type p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (SkolemMotive p)
deriving instance (Show (Var p), Show (Ann p)) => Show (SkolemMotive p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (SkolemMotive p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (SkolemMotive p)

instance Eq (Var p) => Eq (Skolem p) where
  Skolem v _ _ _ == Skolem v' _ _ _ = v == v'

instance Ord (Var p) => Ord (Skolem p) where
  Skolem v _ _ _ `compare` Skolem v' _ _ _ = v `compare` v'

deriving instance (Show (Var p), Show (Ann p)) => Show (Type p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Type p)
deriving instance Ord (Var p) => Ord (Type p)
deriving instance Eq (Var p) => Eq (Type p)

data Coercion p
  = VarCo (Var p)
  | ReflCo (Type p) -- <T> : T ~ T
  | SymCo (Coercion p) -- sym (X : T ~ S) : S ~ T
  | AppCo (Coercion p) (Coercion p) -- (f : B ~ D) (x : A ~ C) : B A ~ D C
  | ArrCo (Coercion p) (Coercion p) -- (x : S ~ T) -> (y : S' ~ T') : (S -> S') ~ (T -> T')
  | ProdCo (Coercion p) (Coercion p) -- (x : S ~ T, y : S' ~ T') : (S, S') ~ (T, T')
  | ExactRowsCo [(Text, Coercion p)] -- { x : A ~ B } : { x : A } ~ { x : B }
  | RowsCo (Coercion p) [(Text, Coercion p)] -- { x : A ~ B | f : S ~ T } : { A | f : S } ~ { B | f : T }
  | ProjCo [(Text, Type p)] [(Text, Coercion p)]
    -- { x : A ~ B | y : S ~ T } : { x : A, y : S } ~ { x : B | y : T }
  | AssumedCo (Type p) (Type p) -- <A, B> : A ~ B
  | ForallCo (Var p) (Coercion p) (Coercion p)
    -- (forall (v : x : c ~ d). phi : a ~ b) : forall (v : c). a ~ forall (v : d). b

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Coercion p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Coercion p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Coercion p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Coercion p)

data Toplevel p
  = LetStmt [Binding p]
  | ForeignVal (Var p) Text (Type p) (Ann p)
  | TypeDecl (Var p) [TyConArg p] [Constructor p]
  | Module (Var p) [Toplevel p]
  | Open { openName :: Var p
         , openAs :: Maybe T.Text }
  | Class { className :: Var p
          , classCtx :: Maybe (Type p)
          , classParams :: [TyConArg p]
          , classMethods :: [ClassItem p]
          , ann :: Ann p }
  | Instance { instanceClass :: Var p
             , instanceCtx :: Maybe (Type p)
             , instanceHead :: Type p
             , instanceMethods :: [Binding p]
             , ann :: Ann p }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Toplevel p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Toplevel p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Toplevel p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Toplevel p)

data ClassItem p
  = MethodSig { _methName :: Var p
              , _methTy :: Type p
              , _methAnn :: Ann p
              }
  | DefaultMethod { _methodBinding :: Binding p
                  , _methAnn :: Ann p
                  }

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (ClassItem p)
deriving instance (Show (Var p), Show (Ann p)) => Show (ClassItem p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (ClassItem p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (ClassItem p)

data TyConArg p
  = TyVarArg (Var p)
  | TyAnnArg (Var p) (Type p) -- ( 'a : k )

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (TyConArg p)
deriving instance (Show (Var p), Show (Ann p)) => Show (TyConArg p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (TyConArg p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (TyConArg p)

data Constructor p
  = UnitCon (Var p) (Ann p)
  -- In ArgCon, the Type p is the type of the (sole) argument
  | ArgCon (Var p) (Type p) (Ann p)
  -- In GeneralisedCon, the Type p is the type of the overall thing
  | GeneralisedCon (Var p) (Type p) (Ann p)

deriving instance (Eq (Var p), Eq (Ann p)) => Eq (Constructor p)
deriving instance (Show (Var p), Show (Ann p)) => Show (Constructor p)
deriving instance (Ord (Var p), Ord (Ann p)) => Ord (Constructor p)
deriving instance (Data p, Typeable p, Data (Var p), Data (Ann p)) => Data (Constructor p)
instance (Data (Var p), Data (Ann p), Data p) => Spanned (Constructor p)

pattern TyArr :: Type p -> Type p -> Type p
pattern TyArr t t' <- TyPi (Anon t) t' where
  TyArr t ty = TyPi (Anon t) ty

pattern TyForall :: Var p -> Maybe (Type p) -> Type p -> Type p
pattern TyForall v k t' <- TyPi (Invisible v k) t' where
  TyForall v k ty = TyPi (Invisible v k) ty

getType :: Data (f Typed) => f Typed -> Type Typed
getType = snd . head . catMaybes . gmapQ get where
  get d = fmap (`asTypeOf` (undefined :: (Span, Type Typed))) (cast d)
  -- FIXME: Point-freeing this definition makes type inference broken.
  -- Thanks, GHC.

makePrisms ''Expr
makePrisms ''Type
makePrisms ''Arm
makePrisms ''Pattern
makePrisms ''Toplevel
makePrisms ''TyBinder
makePrisms ''Constructor
makePrisms ''Lit

makeLenses ''Field
makeLenses ''Skolem
makeLenses ''TyBinder
makeLenses ''Binding
makeLenses ''Parameter
makeLenses ''ClassItem

instance Spanned (Ann p) => Spanned (Binding p) where
  annotation = annotation . _bindAnn

instance (Spanned (Constructor p), Spanned (Ann p)) => Spanned (Toplevel p) where
  annotation (LetStmt [b]) = annotation b
  annotation (LetStmt (b:vs)) = sconcat (annotation b :| map annotation vs)
  annotation (TypeDecl _ _ (x:xs)) = sconcat (annotation x :| map annotation xs)
  annotation (ForeignVal _ _ _ x) = annotation x
  annotation (Class _ _ _ _ x) = annotation x
  annotation (Instance _ _ _ _ x) = annotation x
  annotation _ = internal

_TyArr :: Prism' (Type p) (Type p, Type p)
_TyArr = prism (uncurry (TyPi . Anon)) go where
  go (TyArr a b) = Right (a, b)
  go x = Left x

isSkolemisable :: Type Typed -> Bool
isSkolemisable (TyPi Invisible{} _) = True
isSkolemisable (TyPi Implicit{} _) = True
isSkolemisable _ = False

mkWildTy :: Maybe (Type p) -> Type p
mkWildTy (Just x@(TyWildcard _)) = x
mkWildTy t = TyWildcard t

instance Spanned (Ann p) => Spanned (Expr p) where
  annotation (VarRef _ a) = annotation a
  annotation (Let _ _ a) = annotation a
  annotation (If _ _ _ a) = annotation a
  annotation (App _ _ a) = annotation a
  annotation (Fun _ _ a) = annotation a
  annotation (Begin _ a) = annotation a
  annotation (Literal _ a) = annotation a
  annotation (Match _ _ a) = annotation a
  annotation (Function _ a) = annotation a
  annotation (BinOp _ _ _ a) = annotation a
  annotation (Hole _ a) = annotation a
  annotation (Ascription _ _ a) = annotation a

  annotation (Record _ a) = annotation a
  annotation (RecordExt _ _ a) = annotation a
  annotation (Access _ _ a) = annotation a

  annotation (LeftSection _ _ a) = annotation a
  annotation (RightSection _ _ a) = annotation a
  annotation (BothSection _ a) = annotation a
  annotation (AccessSection _ a) = annotation a
  annotation (Parens _ a) = annotation a

  annotation (Tuple _ a) = annotation a
  annotation (TupleSection _ a) = annotation a

  annotation (OpenIn _ _ a) = annotation a
  annotation (Lazy _ a) = annotation a
  annotation (Vta _ _ a) = annotation a
  annotation (ListExp _ a) = annotation a
  annotation (ListComp _ _ a) = annotation a

  annotation (ExprWrapper _ _ a) = annotation a

instance Spanned (Ann p) => Spanned (Pattern p) where
  annotation (Wildcard a) = annotation a
  annotation (Capture _ a) = annotation a
  annotation (Destructure _ _ a) = annotation a
  annotation (PAs _ _ a) = annotation a
  annotation (PType _ _ a) = annotation a
  annotation (PRecord _ a) = annotation a
  annotation (PTuple _ a) = annotation a
  annotation (PLiteral _ a) = annotation a
  annotation (PWrapper _ _ a) = annotation a
  annotation (PSkolem _ _ a) = annotation a
  annotation (PList _ a) = annotation a

instance Spanned (Ann p) => Spanned (Arm p) where
  annotation (Arm p _ e) = annotation p <> annotation e

instance Spanned (Ann p) => Spanned (Parameter p) where
  annotation = annotation . view paramPat

instance Spanned (Ann p) => Spanned (ClassItem p) where
  annotation = annotation . view methAnn

{- Note [1]: Tuple types vs tuple patterns/values

    Tuple types only describe *pairs*, but patterns/values can have any
    number of elements. We do this like Idris, in which (a, b, c) = (a,
    (b, c)) -}

--- vim: fdm=marker
