module HS.Elab (elabWith, elab, elabPat, elabRuleDecl) where

import ApplicativeTRS.Signature
import HS.Syntax
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

elabPat :: Pat -> Term
elabPat (PVar x) = V (packName x)
elabPat (PCon c ps) = F (packName c) [elabPat p | p <- ps]
elabPat PWild = error "elabPat: uncovered wild"

elabRuleDecl :: Signature -> RuleDecl -> Rule
elabRuleDecl sig rd =
  ( F (packName (rdName rd)) [elabPat p | p <- rdPats rd],
    elab sig (rdBody rd)
  )
