module Suite.Laws.Strategy (tests) where

import Data.List (isPrefixOf, isSubsequenceOf)
import Suite.Gen.Term
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
import TRS

strategies :: [(String, Strategy)]
strategies =
  [ ("leftmost-outermost", leftmostOutermost),
    ("leftmost-innermost", leftmostInnermost),
    ("parallel-outermost", parallelOutermost)
  ]

cap :: Int
cap = 200

tests :: TestTree
tests = testGroup "TRS strategy laws" (perStrategy ++ shared ++ indexAndPrune ++ zipperAgreement)

perStrategy :: [TestTree]
perStrategy =
    [ testGroup
        name
        [ testProperty "a normal outcome really is a normal form" $
            \(SmallTRS trs) (GroundTerm t) ->
              case traceWith st cap trs t of
                Trace _ u Normal -> property (null (reducts trs u))
                Trace {} -> label "limit" True,
          testProperty "every redex position is a real position" $
            \(SmallTRS trs) (GroundTerm t) ->
              conjoin
                [ property (rxPos rx `elem` positions before)
                | (before, st') <- walk trs t,
                  rx <- stepRedexes st'
                ],
          testProperty "traceWith is a prefix of the unbounded trace" $
            \(SmallTRS trs) (GroundTerm t) (NonNegative n) ->
              let k = min n 50
               in map stepResult (trSteps (traceWith st k trs t))
                    === take k (map stepResult (trSteps (traceWith st cap trs t))),
          testProperty "traceWith reports Normal iff the trace fits" $
            \(SmallTRS trs) (GroundTerm t) (NonNegative n) ->
              let k = min n 50
                  full = trSteps (traceWith st cap trs t)
               in length full < cap ==>
                    ((trOutcome (traceWith st k trs t) == Normal) === (length full <= k)),
          testProperty "the final term is the last step's result" $
            \(SmallTRS trs) (GroundTerm t) ->
              let tr = traceWith st cap trs t
               in trTerm tr === lastOr t (map stepResult (trSteps tr))
        ]
    | (name, st) <- strategies
    ]

zipperAgreement :: [TestTree]
zipperAgreement =
  [ testProperty "the zipper normaliser agrees with the generic loop" $
      \(SmallTRS trs) (GroundTerm t) ->
        let a = normaliseOutermost cap trs t
            b = normaliseWith leftmostOutermost cap trs t
         in (nrTerm a, nrSteps a, nrOutcome a) === (nrTerm b, nrSteps b, nrOutcome b),
    testProperty "the zipper normaliser respects its limit" $
      \(SmallTRS trs) (GroundTerm t) (NonNegative k) ->
        let n = min k 30
            r = normaliseOutermost n trs t
         in property (nrSteps r <= n),
    testProperty "nf agrees with the generic loop" $
      \(SmallTRS trs) (GroundTerm t) ->
        let b = normaliseWith leftmostOutermost cap trs t
         in nrOutcome b == Normal ==> (nf trs t === nrTerm b)
  ]

indexAndPrune :: [TestTree]
indexAndPrune =
  [ testProperty "15-3 P1 findMatchI agrees with the linear scan" $
      \(SmallTRS trs) (SmallTerm t) ->
        findMatchI (indexTRS trs) t === findTRSMatch trs t,
    testProperty "15-3 P3 the index is an order-preserving partition of R" $
      \(SmallTRS trs) ->
        indexedRules (indexTRS trs) === [r | r@(F _ _, _) <- trs],
    testProperty "15-3 P2 indexing does not change the normal form" $
      \(SmallTRS trs) (GroundTerm t) ->
        let tr = traceWith leftmostOutermost cap trs t
         in trOutcome tr == Normal
              ==> (trTerm tr === nf trs t),
    testProperty "15-2 P3 prune returns a subsequence of R" $
      \(SmallTRS trs) (GroundTerm t) ->
        property (isSubsequenceOf (prune trs t) trs),
    testProperty "15-2 P1 pruning does not change the normal form" $
      \(SmallTRS trs) (GroundTerm t) ->
        let full = traceWith leftmostOutermost cap trs t
            cut = traceWith leftmostOutermost cap (prune trs t) t
         in trOutcome full == Normal ==> (trTerm cut === trTerm full),
    testProperty "15-2 P2 pruning does not change the step sequence" $
      \(SmallTRS trs) (GroundTerm t) ->
        let steps r = map stepResult (trSteps (traceWith leftmostOutermost cap r t))
            full = traceWith leftmostOutermost cap trs t
         in trOutcome full == Normal ==> (steps (prune trs t) === steps trs),
    testProperty "15-2 pruning is idempotent" $
      \(SmallTRS trs) (GroundTerm t) ->
        prune (prune trs t) t === prune trs t
  ]

shared :: [TestTree]
shared =
         [ testProperty "nfWithLimit succeeds at exactly the trace length" $
             \(SmallTRS trs) (GroundTerm t) ->
               let full = trSteps (traceWith leftmostOutermost cap trs t)
                in length full < cap ==>
                     (nfWithLimit (length full) trs t === Right (nf trs t)),
           testProperty "parallel-outermost is never slower than leftmost-outermost" $
             \(SmallTRS trs) (GroundTerm t) ->
               let po = traceWith parallelOutermost cap trs t
                   lo = traceWith leftmostOutermost cap trs t
                in (trOutcome po == Normal && trOutcome lo == Normal)
                     ==> property (length (trSteps po) <= length (trSteps lo)),
           testProperty "parallel-outermost redexes are pairwise parallel" $
             \(SmallTRS trs) (GroundTerm t) ->
               conjoin
                 [ property (not (p `isPrefixOf` q) && not (q `isPrefixOf` p))
                 | (_, st') <- walk trs t,
                   let ps = map rxPos (stepRedexes st'),
                   (i, p) <- zip [0 :: Int ..] ps,
                   (j, q) <- zip [0 :: Int ..] ps,
                   i < j
                 ]
         ]

lastOr :: a -> [a] -> a
lastOr d [] = d
lastOr _ us = last us

-- Every (term, step) pair along a parallel-outermost run.
walk :: TRS -> Term -> [(Term, Step)]
walk trs = go (0 :: Int)
  where
    go n t
      | n >= 30 = []
      | otherwise = case parallelOutermost (indexTRS trs) t of
          Nothing -> []
          Just st -> (t, st) : go (n + 1) (stepResult st)
