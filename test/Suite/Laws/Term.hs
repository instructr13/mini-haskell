module Suite.Laws.Term (tests) where

import Data.List (nub, sort)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import TRS.Parser (readTRS)
import Suite.Gen.Term
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import TRS

tests :: TestTree
tests =
  testGroup
    "TRS term laws"
    [ testProperty "root position is always present" $ \(SmallTerm t) ->
        [] `elem` positions t,
      testProperty "positions has no duplicates" $ \(SmallTerm t) ->
        let ps = positions t in length ps === length (nub ps),
      testProperty "p is a position iff subTermAt succeeds" $ \(SmallTerm t) ->
        conjoin [isJust (subTermAt t p) | p <- positions t],
      testProperty "replace then read back" $ \(SmallTerm t) (SmallTerm u) ->
        forAll (elements (positions t)) $ \p ->
          (replace t u p >>= \t' -> subTermAt t' p) === Just u,
      testProperty "replace with what was there is identity" $ \(SmallTerm t) ->
        forAll (elements (positions t)) $ \p ->
          (subTermAt t p >>= \u -> replace t u p) === Just t,
      testProperty "variables = nub . variableOccurrences" $ \(SmallTerm t) ->
        variables t === nub (variableOccurrences t),
      testProperty "repeatedVariables are exactly the non-linear ones" $ \(SmallTerm t) ->
        repeatedVariables t
          === sort [x | x <- variables t, length (filter (== x) (variableOccurrences t)) > 1],
      testProperty "substitute with the empty substitution is identity" $ \(SmallTerm t) ->
        substitute t [] === t,
      testProperty "renameTerm is injective on variables" $ \(SmallTerm t) ->
        variables (renameTerm "'" t) === map (++ "'") (variables t),
      testProperty "compose law" $ \(SmallTerm t) (SubstOf s) (SubstOf u) ->
        substitute t (compose s u) === substitute (substitute t s) u,
      testProperty "match is sound" $ \(LinearPat l) (GroundTerm t) ->
        case match l t of
          Nothing -> label "no match" True
          Just s -> label "match" (substitute l s === t),
      testProperty "match respects arity" $
        match (F "g" [V "x"]) (F "g" [F "0" [], F "0" []]) === Nothing,
      testProperty "unify is sound" $ \(SmallTerm a) (SmallTerm b) ->
        case unify a b of
          Nothing -> label "no unifier" True
          Just s -> label "unifier" (substitute a s === substitute b s),
      testProperty "unifiability is symmetric" $ \(SmallTerm a) (SmallTerm b) ->
        isJust (unify a b) === isJust (unify b a),
      testProperty "occurs check rejects x = f(x)" $
        unify (V "x") (F "s" [V "x"]) === Nothing,
      testProperty "symbolOccurrences agrees with a positional walk" $ \(SmallTerm t) ->
        symbolOccurrences t
          === [(f, length ts) | p <- positions t, Just (F f ts) <- [subTermAt t p]],
      testProperty "definedSymbols are the lhs roots" $ \(SmallTRS trs) ->
        definedSymbols trs === Set.fromList [f | (F f _, _) <- trs],
      testProperty "unifiableApart is symmetric" $ \(SmallTerm a) (SmallTerm b) ->
        unifiableApart a b === unifiableApart b a,
      testProperty "unifiableApart ignores variable names" $ \(SmallTerm a) (SmallTerm b) ->
        unifiableApart a b === unifiableApart a (renameTerm "_zz" b),
      testProperty "16-2 readTRS . showTRS is the identity" $ \(SmallTRS trs) ->
        readTRS (showTRS trs) === Right trs,
      testProperty "16-2 showTRS lists exactly the variables" $ \(SmallTRS trs) ->
        case readTRS (showTRS trs) of
          Left e -> counterexample (show e) False
          Right back -> trsVariables back === trsVariables trs
    ]
