module Suite.Support
  ( canonicalRule,
    canonicalTRS,
    alphaEqTRS,
    RuleDiff (..),
    diagnose,
    renderTRS,
    renderRules,
    parseRules,
    assertRules,
    assertRulesAlpha,
    argsOfRule,
    ruleFor,
  )
where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import Test.Tasty.HUnit (Assertion, assertFailure)
import TRS
import TRS.Parser (readTRS)

canonicalRule :: Rule -> Rule
canonicalRule (l, r) = (rename l, rename r)
  where
    order = variableOccurrences l ++ variableOccurrences r
    table = Map.fromList (zip (dedup order) [1 :: Int ..])
    dedup = foldr keep []
      where
        keep x xs = if x `elem` xs then xs else x : xs
    rename (V x) = V (maybe x (\i -> "_" ++ show i) (Map.lookup x table))
    rename (F f ts) = F f (map rename ts)

canonicalTRS :: TRS -> [String]
canonicalTRS = sort . map (showRule . canonicalRule)

alphaEqTRS :: TRS -> TRS -> Bool
alphaEqTRS a b = canonicalTRS a == canonicalTRS b

renderTRS :: TRS -> String
renderTRS = unlines . map showRule

renderRules :: TRS -> [String]
renderRules = map showRule

parseRules :: [String] -> String -> TRS
parseRules vs body =
  case readTRS ("(VAR " ++ unwords vs ++ ") (RULES " ++ body ++ " )") of
    Left e -> error ("parseRules: " ++ show e)
    Right t -> t

data RuleDiff
  = Identical
  | OrderOnly
  | AlphaOnly
  | Differs Int String String
  deriving (Eq, Show)

diagnose :: TRS -> TRS -> RuleDiff
diagnose want got
  | renderRules want == renderRules got = Identical
  | sort (renderRules want) == sort (renderRules got) = OrderOnly
  | canonicalTRS want == canonicalTRS got = AlphaOnly
  | otherwise = firstDiff 0 (renderRules want) (renderRules got)
  where
    firstDiff i (w : ws) (g : gs)
      | w == g = firstDiff (i + 1) ws gs
      | otherwise = Differs i w g
    firstDiff i (w : _) [] = Differs i w "<missing>"
    firstDiff i [] (g : _) = Differs i "<missing>" g
    firstDiff i [] [] = Differs i "" ""

explain :: RuleDiff -> String
explain Identical = "identical"
explain OrderOnly =
  "rule ORDER differs; a pass is emitting helper rules non-deterministically"
explain AlphaOnly =
  "only variable NAMES differ; check the fresh-name scheme and the extra-argument order"
explain (Differs i w g) =
  "first difference at rule " ++ show i ++ "\n  expected: " ++ w ++ "\n  actual:   " ++ g

report :: String -> TRS -> TRS -> RuleDiff -> Assertion
report name want got d =
  assertFailure $
    name
      ++ ": "
      ++ explain d
      ++ "\n--- expected ---\n"
      ++ renderTRS want
      ++ "--- actual ---\n"
      ++ renderTRS got

assertRules :: String -> TRS -> TRS -> Assertion
assertRules name want got = case diagnose want got of
  Identical -> pure ()
  d -> report name want got d

assertRulesAlpha :: String -> TRS -> TRS -> Assertion
assertRulesAlpha name want got
  | alphaEqTRS want got = pure ()
  | otherwise = report name want got (diagnose want got)

ruleFor :: String -> TRS -> Maybe Rule
ruleFor f trs = case [r | r@(F g _, _) <- trs, g == f] of
  (r : _) -> Just r
  [] -> Nothing

argsOfRule :: String -> TRS -> [Term]
argsOfRule f trs = case ruleFor f trs of
  Just (F _ as, _) -> as
  _ -> []
