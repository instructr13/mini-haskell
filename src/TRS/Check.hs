module TRS.Check (Violation (..), checkRule, checkTRS) where

import Data.List (nub, (\\))
import TRS
import TRS.Match (unify)
import TRS.Rewrite (nfBounded)
import Term

data Violation
  = LhsIsVariable Rule
  | LhsIsApplication Rule
  | UnboundRhsVar Rule String
  | NonLeftLinear Rule String
  | NotConstructorSystem Rule
  | AmbiguousOverlap Rule Rule Term Term
  | UnresolvedOverlap Rule Rule Term Term
  deriving (Show, Eq)

checkLhsIsVariable :: Rule -> [Violation]
checkLhsIsVariable rule@(V _, _) = [LhsIsVariable rule]
checkLhsIsVariable _ = []

-- ok:  map f (x : xs) -> ...
-- bad: map (f x) ys   -> ...
-- bad: f x            -> ...   (f declared in (VAR ...))
checkLhsIsApplication :: Rule -> [Violation]
checkLhsIsApplication rule@(l, _) = [LhsIsApplication rule | containsApp l]
  where
    containsApp :: Term -> Bool
    containsApp (_ :@ _) = True
    containsApp (F _ ts) = any containsApp ts
    containsApp (V _) = False

checkUnboundRhsVar :: Rule -> [Violation]
checkUnboundRhsVar rule@(l, r) = [UnboundRhsVar rule x | x <- variables r \\ variables l]

checkNonLeftLinear :: Rule -> [Violation]
checkNonLeftLinear rule@(l, _) = [NonLeftLinear rule x | x <- nub (variablesWithDups l \\ variables l)]
  where
    variablesWithDups :: Term -> [String]
    variablesWithDups (V x) = [x]
    variablesWithDups (F _ ts) = [x | t <- ts, x <- variablesWithDups t]

-- l1|_p sigma = l2 sigma  =>  <(l1[r2]_p) sigma, r1 sigma>
criticalPairs :: Rule -> Rule -> [(Position, Term, Term)]
criticalPairs (l1, r1) (l20, r20) =
  [ (p, substitute l1' sigma, substitute r1 sigma)
  | p <- positions l1,
    Just u@(F _ _) <- [subTermAt l1 p],
    Just sigma <- [unify u l2],
    Just l1' <- [replace l1 r2 p]
  ]
  where
    l2 = renameTerm "_cp" l20
    r2 = renameTerm "_cp" r20

-- How far a critical pair is normalised before giving up on it.
overlapStepLimit :: Int
overlapStepLimit = 1000

-- ok:  xs ++ [] -> xs  overlaps  (x : xs) ++ ys -> x : (xs ++ ys)
--      but ((x : xs) ++ []) reaches (x : xs) either way
-- bad: f x -> True  overlaps  f 0 -> False
checkCriticalPairs :: TRS -> [Violation]
checkCriticalPairs trs =
  [ v
  | (rule1, i) <- indexed,
    (rule2, j) <- indexed,
    (p, s, t) <- criticalPairs rule1 rule2,
    -- Every rule trivially overlaps itself at the root, and a root overlap of
    -- two rules is the same pair seen twice.
    not (null p) || i < j,
    v <- verdict rule1 rule2 s t
  ]
  where
    indexed = zip trs [0 :: Integer ..]

    verdict rule1 rule2 s t =
      case (nfBounded overlapStepLimit trs s, nfBounded overlapStepLimit trs t) of
        (Just s', Just t')
          | s' == t' -> []
          | otherwise -> [AmbiguousOverlap rule1 rule2 s' t']
        _ -> [UnresolvedOverlap rule1 rule2 s t]

checkRule :: Rule -> [Violation]
checkRule rule =
  checkLhsIsVariable rule
    ++ checkLhsIsApplication rule
    ++ checkUnboundRhsVar rule
    ++ checkNonLeftLinear rule

checkTRS :: TRS -> [Violation]
checkTRS trs = concat [checkRule rule | rule <- trs] ++ checkCriticalPairs trs
