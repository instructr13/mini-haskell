module Suite.Check (tests) where

import HS.Check
import HS.Name
import Suite.Support (parseRules)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import TRS

sigOf :: TRS -> Signature
sigOf = signatureFromTRS

kinds :: [Violation] -> [String]
kinds = map kindOf
  where
    kindOf v = case v of
      LhsIsVariable {} -> "LhsIsVariable"
      UnboundRhsVar {} -> "UnboundRhsVar"
      NonLeftLinear {} -> "NonLeftLinear"
      ArityMismatch {} -> "ArityMismatch"
      NotConstructorSystem {} -> "NotConstructorSystem"
      RootOverlap {} -> "RootOverlap"
      SymbolClash {} -> "SymbolClash"

check :: [String] -> String -> [Violation]
check vs body = let trs = parseRules vs body in checkTRS (sigOf trs) trs

tests :: TestTree
tests =
  testGroup
    "checkTRS"
    [ testCase "a well-formed constructor system has no violations" $
        check ["x", "y", "xs"] "add(0, y) -> y  add(s(x), y) -> s(add(x, y))" @?= [],
      testCase "a variable lhs is reported exactly once" $
        kinds (check ["x"] "x -> a") @?= ["LhsIsVariable"],
      testCase "a variable lhs does not produce spurious root overlaps" $
        kinds (check ["x"] "x -> a  f(x) -> a  g(x) -> a") @?= ["LhsIsVariable"],
      testCase "a repeated pattern variable is non-left-linear" $
        kinds (check ["x"] "g(x, x) -> x") @?= ["NonLeftLinear"],
      testCase "an unbound rhs variable is reported" $
        kinds (check ["x", "y"] "h(x) -> y") @?= ["UnboundRhsVar"],
      testCase "a defined symbol inside a pattern is not a constructor system" $
        kinds (check ["x"] "f(x) -> x  g(f(x)) -> x") @?= ["NotConstructorSystem"],
      testCase "overlapping rules are reported once per pair" $
        kinds (check ["x"] "f(x) -> a  f(x) -> a") @?= ["RootOverlap"],
      testCase "distinct constructor patterns do not overlap" $
        check ["x", "y"] "f(0, y) -> y  f(s(x), y) -> x" @?= [],
      testCase "the wildcard-style overlap the doc warns about is caught" $
        kinds (check ["x"] "f(a) -> 0  f(x) -> s(0)") @?= ["RootOverlap"],
      testCase "the naive literal pattern overlap of 14-3 is caught" $
        kinds (check ["n"] "f(0) -> nil  f(n) -> cons(n, f(pred(n)))") @?= ["RootOverlap"],
      testCase "a violation names the rule it is about" $
        let vs = check ["x"] "f(x) -> a  f(x) -> a"
         in map (fmap atId . violationRule) vs @?= [Just 0],
      testCase "the two rules of an overlap are distinguishable" $
        case check ["x"] "f(x) -> a  f(x) -> a" of
          [RootOverlap a b] -> (atId a, atId b) @?= (0, 1)
          vs -> assertFailure ("expected one RootOverlap, got " ++ show (kinds vs)),
      testCase "using a symbol at two arities is an arity mismatch" $
        kinds (check ["x"] "f(x) -> a  g(x) -> f(x, x)") @?= ["ArityMismatch"],
      testCase "a constructor that is also a defined symbol clashes" $
        let trs = parseRules ["x"] "f(x) -> a  a -> b"
            sig = sigOf trs
         in kinds (checkSymbolClash sig trs) @?= []
    ]
