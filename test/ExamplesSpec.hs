module ExamplesSpec (spec) where

import ApplicativeTRS.Pretty (prettyApplicativeTerm)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Source (compileSrc)
import TRS.Rewrite (nf)
import Term
import Test.Hspec

-- Load an example the way the executable does, and print the normal form of main.
normalFormOfMain :: FilePath -> IO String
normalFormOfMain file = do
  src <- readFile file

  case compileSrc file src of
    Left _ -> pure "<load error>"
    Right trs -> case lookup (F "main" []) trs of
      Nothing -> pure "<no main>"
      Just t -> pure (renderString (layoutPretty defaultLayoutOptions (prettyApplicativeTerm (nf trs t))))

reduces :: FilePath -> String -> Spec
reduces file expected =
  it (file ++ " reduces main to " ++ expected) $
    normalFormOfMain file `shouldReturn` expected

spec :: Spec
spec = describe "examples" $ do
  reduces "examples/trst/add.trst" "3"
  reduces "examples/trst/map.trst" "[2, 3, 4, 5, 6]"
  reduces "examples/trst/qsort.trst" "[0, 1, 2, 3, 4]"
  reduces "examples/trs/add.trs" "3"
  reduces "examples/trs/qsort.trs" "[0, 1, 2, 3, 4]"
