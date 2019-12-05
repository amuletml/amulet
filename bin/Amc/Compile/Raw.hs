module Amc.Compile.Raw (raw) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH

raw :: QuasiQuoter
raw = QuasiQuoter { quoteExp = stringE, quotePat = undefined, quoteType = undefined, quoteDec = undefined }

