module Main (main) where

import Suite.Env
import qualified Suite.Check
import qualified Suite.Examples
import qualified Suite.Golden
import qualified Suite.Laws.Strategy
import qualified Suite.Laws.Term
import qualified Suite.Stage2
import qualified Suite.Stage3
import qualified Suite.Stages
import qualified Suite.Traverse
import Test.Tasty

main :: IO ()
main = do
  stage <- currentStage
  let env = TestEnv "." stage
  found <- discoverExamples env
  defaultMain $
    localOption (mkTimeout (60 * 1000 * 1000)) $
      testGroup
        "mini-haskell"
        [ Suite.Laws.Term.tests,
          Suite.Check.tests,
          Suite.Traverse.tests,
          Suite.Laws.Strategy.tests,
          Suite.Stage2.tests,
          Suite.Stage3.tests,
          Suite.Stages.tests env,
          Suite.Examples.tests env found,
          Suite.Golden.tests env
        ]
