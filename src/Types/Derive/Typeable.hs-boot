module Types.Derive.Typeable (deriveTypeable, builtinTypeableInsts) where

import Syntax.Implicits (ImplicitScope)
import Syntax.Types (DerivingStrat, ClassInfo)
import Syntax.Var (Typed)

deriveTypeable :: DerivingStrat
builtinTypeableInsts :: ImplicitScope ClassInfo Typed
