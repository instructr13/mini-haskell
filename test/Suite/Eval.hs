module Suite.Eval
  ( Run (..),
    defaultLimit,
    loopLimit,
    runWith,
    runFrom,
    runMainOf,
    runMainWith,
    mainTermOf,
    assertNormalises,
  )
where

import Test.Tasty.HUnit (Assertion, assertFailure)
import TRS

defaultLimit :: Int
defaultLimit = 100000

loopLimit :: Int
loopLimit = 2000

data Run = Run
  { runFinal :: Term,
    runStepCount :: Int,
    runNormal :: Bool
  }
  deriving (Eq, Show)

-- The step-count convention used by every reference figure in the tests:
-- start from F "main" [] and count each rewrite, leftmost-outermost.
runWith :: Strategy -> Int -> TRS -> Term -> Run
runWith st limit trs t0 = case traceWith st (max 1 limit) trs t0 of
  tr -> Run (trTerm tr) (length (trSteps tr)) (trOutcome tr == Normal)

runFrom :: Int -> TRS -> Term -> Run
runFrom = runWith leftmostOutermost

mainTermOf :: Term
mainTermOf = F "main" []

runMainOf :: Int -> TRS -> Run
runMainOf limit trs = runFrom limit trs mainTermOf

runMainWith :: Strategy -> Int -> TRS -> Run
runMainWith st limit trs = runWith st limit trs mainTermOf

assertNormalises :: String -> Run -> Assertion
assertNormalises name r
  | runNormal r = pure ()
  | otherwise =
      assertFailure $
        name
          ++ ": did not normalise within the limit after "
          ++ show (runStepCount r)
          ++ " steps; stopped at "
          ++ show (runFinal r)
