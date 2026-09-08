module Suite.Examples (tests) where

import Data.List (sort)
import HS.Check (checkTRS)
import HS.Name (signatureFromTRS)
import Suite.Env
import Suite.Eval
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit

-- Every .hs under examples/ must compile, pass checkTRS, and (if it defines
-- main) reach a normal form. Sources that are meant to fail live in test/data/.
tests :: TestEnv -> [FilePath] -> TestTree
tests env found =
  testGroup
    "examples smoke test"
    [ testCase "the discovered set matches the expected table" $
        sort (map exPath expectedExamples) @?= sort (map (drop (length (envRoot env) + 1)) found),
      testGroup
        "each example"
        [ testCase rel $ do
            r <- compileEither env rel
            case r of
              Left e -> assertFailure ("compile error: " ++ show e)
              Right trs -> do
                checkTRS (signatureFromTRS trs) trs @?= []
                case lookup mainTermOf trs of
                  Nothing -> pure ()
                  Just body -> assertNormalises rel (runFrom defaultLimit trs body)
        | ExampleSpec rel stage <- expectedExamples,
          envStage env >= stage
        ]
    ]
