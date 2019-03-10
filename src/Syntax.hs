-- | Re-exports the various syntax modules.
module Syntax (module M) where

import Syntax.Expr.Instances ()
import Syntax.Toplevel as M
import Syntax.Pretty as M
import Syntax.Type as M hiding (record, prettyRows)
import Syntax.Expr as M
import Syntax.Var as M
