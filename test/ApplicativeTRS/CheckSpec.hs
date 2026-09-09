module ApplicativeTRS.CheckSpec (spec) where

import ApplicativeTRS.Loader (loadApplicativeTRS)
import Data.Either (isRight)
import Data.List (isInfixOf)
import TRS.Error (TRSError (..))
import Test.Hspec

-- Load a source snippet through the whole applicative pipeline, prelude included.
run :: String -> Either TRSError String
run src = fmap (const "ok") (loadApplicativeTRS "<test>" src)

shouldReport :: String -> String -> Expectation
shouldReport src msg = case run src of
  Left (Invalid m) | msg `isInfixOf` m -> pure ()
  other -> expectationFailure ("expected " ++ show msg ++ ", got " ++ show other)

spec :: Spec
spec = do
  describe "checkConstructors" $ do
    it "reports an undeclared constructor" $
      "(RULES\n  len Nil -> Nil ;\n  main -> len Nul ;\n)"
        `shouldReport` "undeclared constructor Nul"

    it "reports a constructor applied to more arguments than declared" $
      "(RULES\n  f x -> Cons x Nil Nil ;\n  main -> f Nil ;\n)"
        `shouldReport` "constructor Cons is applied to 3 argument(s) but declared with arity 2"

    it "reports a constructor at the root of a left-hand side" $
      "(RULES\n  Cons x xs -> x ;\n  main -> Nil ;\n)"
        `shouldReport` "constructor Cons cannot be redefined as a rule head"

    it "reports a constructor declared twice" $
      "(DATA Stack = Nil | Push Stack)\n(RULES\n  main -> Nil ;\n)"
        `shouldReport` "constructor Nil is declared more than once"

    it "accepts a partially applied constructor" $
      -- map Succ [1, 2, 3] passes Succ as a value; mkApp saturates it later.
      run
        "(RULES\n\
        \  map f []       -> [] ;\n\
        \  map f (x : xs) -> f x : map f xs ;\n\
        \  main -> map Succ [1, 2, 3] ;\n\
        \)"
        `shouldSatisfy` isRight

  describe "the lexical rule" $ do
    it "catches a constructor written in lower case, via the overlap it creates" $
      "(RULES\n  plus zero y -> y ;\n  plus (Succ x) y -> Succ (plus x y) ;\n  main -> plus Zero Zero ;\n)"
        `shouldReport` "ambiguous rules"

    it "catches a lower case constructor applied in a pattern" $
      "(RULES\n  plus (nat x) y -> Succ (plus x y) ;\n  main -> plus Zero Zero ;\n)"
        `shouldReport` "variable applied to arguments"

    it "needs no declaration for variables" $
      run "(RULES\n  swap (Cons x (Cons y ys)) -> Cons y (Cons x ys) ;\n  main -> swap [1, 2] ;\n)"
        `shouldSatisfy` isRight
