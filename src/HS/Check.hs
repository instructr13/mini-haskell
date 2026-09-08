module HS.Check (module HS.Check) where

import Data.Function (on)
import Data.List (groupBy, tails, (\\))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HS.Name
import TRS

-- A rule together with its index, so that a violation can name the rule it is
-- about even when two rules are textually identical.
data RuleAt = RuleAt
  { atId :: !RuleId,
    atRule :: !Rule
  }
  deriving (Eq, Show)

indexRules :: TRS -> [RuleAt]
indexRules trs = [RuleAt i r | (i, r) <- zip [0 ..] trs]

data Violation
  = LhsIsVariable RuleAt
  | UnboundRhsVar RuleAt String
  | NonLeftLinear RuleAt String
  | ArityMismatch RuleAt String Int Int
  | NotConstructorSystem RuleAt String
  | RootOverlap RuleAt RuleAt
  | SymbolClash String
  deriving (Eq, Show)

violationRule :: Violation -> Maybe RuleAt
violationRule v = case v of
  LhsIsVariable a -> Just a
  UnboundRhsVar a _ -> Just a
  NonLeftLinear a _ -> Just a
  ArityMismatch a _ _ _ -> Just a
  NotConstructorSystem a _ -> Just a
  RootOverlap a _ -> Just a
  SymbolClash _ -> Nothing

-- One classification of the left-hand side, so a bare-variable lhs cannot be
-- reported both as LhsIsVariable and as NotConstructorSystem.
checkLhsShape :: Set.Set String -> RuleAt -> [Violation]
checkLhsShape defined at = case atRule at of
  (V _, _) -> [LhsIsVariable at]
  (F _ ts, _) ->
    [NotConstructorSystem at f | f <- take 1 (concatMap definedIn ts)]
  where
    definedIn t = [g | (g, _) <- symbolOccurrences t, Set.member g defined]

checkUnboundRhsVar :: RuleAt -> [Violation]
checkUnboundRhsVar at = [UnboundRhsVar at x | x <- variables r \\ variables l]
  where
    (l, r) = atRule at

checkNonLeftLinear :: RuleAt -> [Violation]
checkNonLeftLinear at = [NonLeftLinear at x | x <- repeatedVariables (fst (atRule at))]

-- The first rule exhibiting each (symbol, arity) pair.
firstUse :: TRS -> Map (String, Int) RuleAt
firstUse trs =
  foldl' add Map.empty
    [ ((f, n), at)
    | at <- indexRules trs,
      let (l, r) = atRule at,
      t <- [l, r],
      (f, n) <- symbolOccurrences t
    ]
  where
    add m (k, at) = Map.insertWith keepFirst k at m
    keepFirst _new old = old

-- Map.toAscList is ordered by (symbol, arity), so the head of each group
-- carries the smallest arity observed for that symbol.
checkArityMismatch :: Signature -> TRS -> [Violation]
checkArityMismatch sig trs =
  concatMap group (groupBy ((==) `on` (fst . fst)) (Map.toAscList (firstUse trs)))
  where
    group [] = []
    group entries@(((f, smallest), _) : _) =
      [ArityMismatch at f expected n | ((_, n), at) <- entries, n /= expected]
      where
        expected = maybe smallest symArity (Map.lookup f sig)

-- Grouping by root symbol first turns this from a scan over every pair of
-- rules into a scan within each function, and structurally excludes rules
-- whose lhs is a variable (which would otherwise unify with everything).
checkRootOverlap :: TRS -> [Violation]
checkRootOverlap trs =
  [ RootOverlap a b
  | rules <- Map.elems (byRoot (indexRules trs)),
    (a : rest) <- tails rules,
    b <- rest,
    unifiableApart (fst (atRule a)) (fst (atRule b))
  ]
  where
    byRoot = foldr add Map.empty
    add at m = case atRule at of
      (F f _, _) -> Map.insertWith (++) f [at] m
      _ -> m

-- A symbol may not be both a constructor of the signature and the root of a
-- rule; without this, "root overlap is the only possible overlap" fails.
checkSymbolClash :: Signature -> TRS -> [Violation]
checkSymbolClash sig trs =
  [ SymbolClash f
  | f <- Set.toAscList (Set.intersection (definedSymbols trs) (constructorSymbols sig))
  ]

describeViolation :: Violation -> String
describeViolation v = detail ++ at
  where
    at = case violationRule v of
      Just a -> " in rule #" ++ show (atId a) ++ " (" ++ showRule (atRule a) ++ ")"
      Nothing -> ""
    detail = case v of
      LhsIsVariable _ -> "variable left-hand side"
      UnboundRhsVar _ x -> "unbound right-hand side variable " ++ x
      NonLeftLinear _ x -> "non-linear pattern variable " ++ x
      ArityMismatch _ f m n ->
        f ++ " used at arity " ++ show n ++ " but declared at " ++ show m
      NotConstructorSystem _ f -> "defined symbol " ++ f ++ " occurs in a pattern"
      RootOverlap _ b -> "overlaps rule #" ++ show (atId b)
      SymbolClash f -> f ++ " is both a constructor and a defined symbol"

-- Per-rule violations come first: they are the most local diagnosis, and
-- callers that report only the head of the list should see those.
checkTRS :: Signature -> TRS -> [Violation]
checkTRS sig trs =
  concatMap perRule rules
    ++ checkSymbolClash sig trs
    ++ checkArityMismatch sig trs
    ++ checkRootOverlap trs
  where
    rules = indexRules trs
    defined = definedSymbols trs
    perRule at =
      checkLhsShape defined at
        ++ checkNonLeftLinear at
        ++ checkUnboundRhsVar at
