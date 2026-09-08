module Suite.Golden (goldenRules, tests) where

import qualified Data.ByteString.Lazy.Char8 as BL
import Suite.Env
import Suite.Support (renderTRS)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)

-- Update with:  stack test --ta '--accept -p <pattern>'
goldenRules :: TestEnv -> String -> FilePath -> FilePath -> TestTree
goldenRules env name srcRel goldRel =
  goldenVsStringDiff name differ (envRoot env </> goldRel) act
  where
    differ ref new = ["diff", "-u", ref, new]
    act = do
      r <- compileEither env srcRel
      either (fail . show) (pure . BL.pack . renderTRS) r

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "golden rule sets"
    [ goldenRules env "Prelude/Base.hs" "examples/Prelude/Base.hs" "test/golden/prelude-base.trs.golden"
    ]
