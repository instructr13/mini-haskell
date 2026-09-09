module HS.Signature (module HS.Signature) where

import ApplicativeTRS.Signature
import Data.List (nub)
import HS.Syntax

mkSignature :: Module -> Signature
mkSignature m =
  Signature
    { sigCons = [(cdName c, cdArity c) | d <- mData m, c <- ddCons d],
      sigDefined = nub [rdName r | r <- mRules m]
    }
