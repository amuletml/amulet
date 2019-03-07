-- | Re-exports the various syntax modules.
module Syntax (module M) where

import Syntax.Toplevel as M
import Syntax.Pretty as M
import Syntax.Type as M hiding (record, prettyRows)
import Syntax.Expr as M hiding (prettyRows')
import Syntax.Var as M
