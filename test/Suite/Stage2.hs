module Suite.Stage2 (tests) where

import Data.List (isInfixOf)
import HS.Error (CompileError (..))
import HS.Fixity
import HS.Parser (parseHS)
import HS.Pretty (prettyModule)
import Render (renderOneLine)
import Suite.Gen.OpChain
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

resolveSource :: String -> Either CompileError String
resolveSource src = do
  m <- parseHS id "test.hs" src
  env <- collectFixities m
  m' <- resolveModule env m
  pure (renderOneLine (prettyModule m'))

-- (P2)..(P5): a table of fixity declarations, an expression, and the tree the
-- Haskell 2010 Report requires.
resolveCases :: [(String, String, String)]
resolveCases =
  [ ("P2 mixed precedence", "infixl 6 + ;\ninfixl 7 * ;\nmain = 1 + 2 * 3 ;\n", "(+) 1 ((*) 2 3)"),
    ("P3 left associativity", "infixl 6 - ;\nmain = 1 - 2 - 3 ;\n", "(-) ((-) 1 2) 3"),
    ("P4 right associativity", "infixr 5 : ;\nmain = 1 : 2 : Nil ;\n", "(:) 1 ((:) 2 Nil)"),
    ("right associativity of ++", "infixr 5 ++ ;\nmain = a ++ b ++ c ;\n", "(++) a ((++) b c)"),
    ("undeclared operator defaults to infixl 9", "main = a <?> b <?> c ;\n", "(<?>) ((<?>) a b) c")
  ]

tests :: TestTree
tests =
  testGroup
    "Stage 2 - infix operators and fixity"
    [ testGroup
        "resolution table"
        [ testCase name $ case resolveSource src of
            Left e -> assertFailure ("compile error: " ++ show e)
            Right out ->
              assertBool
                (name ++ ": expected " ++ show want ++ " in\n" ++ out)
                (want `isInfixOf` out)
        | (name, src, want) <- resolveCases
        ],
      testCase "P5 mixing infixl and infixr at equal precedence is a conflict" $
        case resolveSource "infixl 5 <+> ;\ninfixr 5 <*> ;\nmain = a <+> b <*> c ;\n" of
          Left (FixityConflict a b) ->
            assertBool
              ("conflict should name both operators, got " ++ show (a, b))
              ("<+>" `isInfixOf` a && "<*>" `isInfixOf` b)
          Left e -> assertFailure ("wrong error: " ++ show e)
          Right out -> assertFailure ("expected FixityConflict, got " ++ out),
      testCase "a non-associative operator cannot chain with itself" $
        case resolveSource "infix 4 == ;\nmain = a == b == c ;\n" of
          Left (FixityConflict _ _) -> pure ()
          Left e -> assertFailure ("wrong error: " ++ show e)
          Right out -> assertFailure ("expected FixityConflict, got " ++ out),
      testProperty "P1 resolution keeps the operand/operator sequence" $
        \(OpChain c) ->
          case resolveFixity testFixities c of
            Left _ -> label "fixity conflict" True
            Right c' -> label "resolved" (flattenChain c' === flattenChain c),
      testProperty "P1 resolution leaves no EOpChain behind" $
        \(OpChain c) ->
          case resolveFixity testFixities c of
            Left _ -> label "fixity conflict" True
            Right c' -> label "resolved" (property (noChain c'))
    ]
