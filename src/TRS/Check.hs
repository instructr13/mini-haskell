module TRS.Check (module TRS.Check) where

import Data.List (nub, (\\))
import Data.Maybe (isJust)
import TRS
import Term

data Violation
  = LhsIsVariable Rule
  | UnboundRhsVar Rule String
  | NonLeftLinear Rule String
  | NotConstructorSystem Rule
  | RootOverlap Rule Rule
  deriving (Show, Eq)

checkLhsIsVariable :: Rule -> [Violation]
checkLhsIsVariable rule@(V _, _) = [LhsIsVariable rule]
checkLhsIsVariable _ = []

checkUnboundRhsVar :: Rule -> [Violation]
checkUnboundRhsVar rule@(l, r) = [UnboundRhsVar rule x | x <- variables r \\ variables l]

checkNonLeftLinear :: Rule -> [Violation]
checkNonLeftLinear rule@(l, _) = [NonLeftLinear rule x | x <- nub (variablesWithDups l \\ variables l)]
  where
    variablesWithDups :: Term -> [String]
    variablesWithDups (V x) = [x]
    variablesWithDups (F _ ts) = [x | t <- ts, x <- variablesWithDups t]

checkNotConstructorSystem :: [String] -> Rule -> [Violation]
checkNotConstructorSystem _ rule@(V _, _) = [NotConstructorSystem rule]
checkNotConstructorSystem syms rule@(F _ ts, _) = if any containsDefined ts then [NotConstructorSystem rule] else []
  where
    -- Is any of the specified symbols D in the term?
    containsDefined :: Term -> Bool
    containsDefined (V _) = False
    containsDefined (F f ts') = (f `elem` syms) || any containsDefined ts'

checkRootOverlap :: TRS -> [Violation]
checkRootOverlap trs =
  [ RootOverlap rule1 rule2
  | (rule1@(l1, _), i) <- zip trs [0 :: Integer ..],
    (rule2@(l2, _), j) <- zip trs [0 :: Integer ..],
    i < j,
    isJust (unify l1 (renameTerm "_rn" l2))
  ]

checkRule :: [String] -> Rule -> [Violation]
checkRule syms rule = checkLhsIsVariable rule ++ checkUnboundRhsVar rule ++ checkNonLeftLinear rule ++ checkNotConstructorSystem syms rule

checkTRS :: TRS -> [Violation]
checkTRS trs = checkRootOverlap trs ++ concat [checkRule syms rule | rule <- trs]
  where
    syms = definedSymbols trs
