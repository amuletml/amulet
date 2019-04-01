{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, OverloadedStrings
  , StandaloneDeriving, TemplateHaskell, TypeFamilies
  , UndecidableInstances #-}

-- | The core types to represent expressions within Amulet's syntax.
module Syntax.Expr where

import Control.Lens hiding (Lazy, (:>))

import Data.Text (Text)
import Data.Typeable
import Data.Data

import Syntax.Type
import Syntax.Var

-- | A literal value
data Lit
  = LiFloat Double
  | LiInt Integer
  | LiStr Text
  | LiBool Bool
  | LiUnit
  deriving (Eq, Show, Ord, Data, Typeable)

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

  | For (Var p, Expr p) (Expr p) (Expr p) (Expr p) (Ann p)
  | While (Expr p) (Expr p) (Ann p)

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
  | TypeAsc (Type p) -- ^ Invisible (to pretty-printer) ascription
  | WrapVar (Var p) -- ^ Unsolved wrapper variable
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

makeLenses ''Parameter
makeLenses ''Binding
makeLenses ''Field

value :: Expr a -> Bool
value Fun{} = True
value Literal{} = True
value Function{} = True
value (Let _ e _) = value e
value (Parens e _) = value e
value Tuple{} = True
value (Begin es _) = value (last es)
value Lazy{} = True
value TupleSection{} = True
value Record{} = True
value RecordExt{} = True
value VarRef{} = False
value If{} = False
value App{} = False
value Match{} = False
value BinOp{} = False
value Hole{} = False
value (Ascription e _ _) = value e
value (Vta e _ _) = value e
value Access{} = False
value LeftSection{} = True
value RightSection{} = True
value BothSection{} = True
value AccessSection{} = True
value ListExp{} = True
value ListComp{} = False
value (OpenIn _ e _) = value e
value (ExprWrapper _ e _) = value e
value For{} = False
value While{} = False

isFn :: Expr a -> Bool
isFn Fun{} = True
isFn (OpenIn _ e _) = isFn e
isFn (Ascription e _ _) = isFn e
isFn (ExprWrapper _ e _) = isFn e
isFn _ = False
