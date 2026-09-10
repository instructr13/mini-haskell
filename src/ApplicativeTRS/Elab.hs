module ApplicativeTRS.Elab (elabWith, elab, elabRule) where

import ApplicativeTRS.Signature
import ApplicativeTRS.Syntax
import TRS
import Term

elabWith :: (String -> SymKind) -> SExpr -> Term
elabWith kindOf = go
  where
    go e
      | Variable <- kindOf x = V (packName x) `applyTo` args
      | otherwise = F (packName x) args
      where
        (x, es) = spine e
        args = map go es

elab :: Signature -> SExpr -> Term
elab = elabWith . classify

elabRule :: Signature -> AppRule -> Rule
elabRule sig (l, r) = (elab sig l, elab sig r)
