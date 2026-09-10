{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module ApplicativeTRS.ElabSpec (spec) where

import ApplicativeTRS.Elab
import ApplicativeTRS.Signature
import ApplicativeTRS.Syntax
import TRS (pattern (:@))
import Term
import Test.Hspec

-- Only "f" is a variable; everything else is a symbol.
kindOf :: String -> SymKind
kindOf "f" = Variable
kindOf _ = Function

spec :: Spec
spec = do
  describe "elabWith" $ do
    it "turns a symbol into F" $
      elabWith kindOf (SEIdent "add") `shouldBe` F "add" []

    it "turns an unapplied variable into V" $
      elabWith kindOf (SEIdent "f") `shouldBe` V "f"

    it "lets a symbol absorb its arguments" $
      elabWith kindOf (SEApp (SEIdent "add") (SEIdent "x"))
        `shouldBe` F "add" [F "x" []]

    it "leaves an applied variable as an @ spine" $
      elabWith kindOf (SEApp (SEIdent "f") (SEIdent "x"))
        `shouldBe` V "f" :@ F "x" []

    it "builds the @ spine left to right" $
      elabWith kindOf (SEApp (SEApp (SEIdent "f") (SEIdent "x")) (SEIdent "y"))
        `shouldBe` V "f" :@ F "x" [] :@ F "y" []

    it "elaborates arguments recursively" $
      elabWith kindOf (SEApp (SEIdent "add") (SEApp (SEIdent "Succ") (SEIdent "f")))
        `shouldBe` F "add" [F "Succ" [V "f"]]

  describe "elab" $ do
    -- map f (Cons x xs) -> ...
    let sig =
          mkSignature
            AppModule
              { amData = [DataDecl "List" [ConDecl "Nil" 0, ConDecl "Cons" 2]],
                amRules =
                  [ ( SEApp (SEApp (SEIdent "map") (SEIdent "f")) (SEIdent "Nil"),
                      SEIdent "Nil"
                    )
                  ]
              }

    it "keeps a higher-order pattern first order under the rule head" $
      elab sig (SEApp (SEApp (SEIdent "map") (SEIdent "f")) (SEIdent "Nil"))
        `shouldBe` F "map" [V "f", F "Nil" []]
