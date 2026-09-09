module ApplicativeTRS.SignatureSpec (spec) where

import ApplicativeTRS.Signature
import ApplicativeTRS.Syntax
import Test.Hspec

-- add Zero y -> y
sampleModule :: AppModule
sampleModule =
  AppModule
    { amData = [DataDecl "Nat" [ConDecl "Zero" 0, ConDecl "Succ" 1]],
      amRules = [(SEApp (SEApp (SEIdent "add") (SEIdent "Zero")) (SEIdent "y"), SEIdent "y")]
    }

spec :: Spec
spec = do
  describe "spine" $ do
    it "leaves a bare identifier alone" $
      fst (spine (SEIdent "x")) `shouldBe` "x"

    it "splits a juxtaposition into its head and its arguments" $ do
      let e = SEApp (SEApp (SEIdent "map") (SEIdent "f")) (SEIdent "xs")

      fst (spine e) `shouldBe` "map"
      length (snd (spine e)) `shouldBe` 2

  describe "classify" $ do
    let sig = mkSignature sampleModule

    it "reads an upper case name as a constructor" $
      classify sig "Succ" `shouldBe` Constructor

    it "reads an upper case name as a constructor even when undeclared" $
      classify sig "Cosn" `shouldBe` Constructor

    it "reads the head of a left-hand side as a defined symbol" $
      classify sig "add" `shouldBe` Function

    it "reads any other lower case name as a variable" $
      classify sig "y" `shouldBe` Variable

  describe "mkSignature" $ do
    let sig = mkSignature sampleModule

    it "collects constructors with their declared arity" $
      lookup "Succ" (sigCons sig) `shouldBe` Just 1

    it "collects D(R) from the roots of the left-hand sides" $
      sigDefined sig `shouldBe` ["add"]

  describe "isConName" $ do
    it "is false for the empty name" $
      isConName "" `shouldBe` False

    it "only looks at the first character" $ do
      isConName "xsY" `shouldBe` False
      isConName "Xs" `shouldBe` True
