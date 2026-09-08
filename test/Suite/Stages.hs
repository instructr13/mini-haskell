module Suite.Stages (tests) where

import Data.List (isPrefixOf)
import qualified Data.Map.Strict as Map
import HS.Compile
import HS.Defunc (applyName)
import HS.Error
import HS.Monad (coreOnly)
import HS.Name (KnownName (..), unVarName)
import Suite.Decode
import Suite.Env
import Suite.Eval
import Suite.Support
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Render (renderOneLine)
import Source (haskellNotation)
import TRS
import TRS.Pretty

-- The step-count convention: from F "main" [], leftmost-outermost. Tagged
-- [refstep] so that `--ta '-p "!/refstep/"'` skips them while iterating.
refSteps :: TestEnv -> String -> FilePath -> Int -> TestTree
refSteps env name rel want =
  testCase ("[refstep] " ++ name) $ do
    trs <- compileOrFail env rel
    let r = runMainWith leftmostOutermost defaultLimit trs
    assertNormalises rel r
    assertEqual
      ( "reference figure (leftmost-outermost, from main); update "
          ++ "test/Suite/Stages.hs if the change was intended"
      )
      want
      (runStepCount r)

normalFormIs :: TestEnv -> String -> FilePath -> Value -> TestTree
normalFormIs env name rel want =
  testCase name $ do
    trs <- compileOrFail env rel
    let r = runMainWith leftmostOutermost defaultLimit trs
    assertNormalises rel r
    decode (runFinal r) @?= want

-- 10.5 / 12 (P4): the two strategies must agree wherever both terminate.
-- Not applicable to the laziness sources, where innermost necessarily
-- diverges because it is a strict strategy.
strategiesAgree :: TestEnv -> FilePath -> TestTree
strategiesAgree env rel =
  testCase (rel ++ " agrees under all strategies") $ do
    trs <- compileOrFail env rel
    let runs = [runMainWith s defaultLimit trs | s <- [leftmostOutermost, leftmostInnermost, parallelOutermost]]
    mapM_ (assertNormalises rel) runs
    case map (decode . runFinal) runs of
      (v : vs) -> mapM_ (v @?=) vs
      [] -> pure ()

nats :: [Integer] -> Value
nats = VList . map VNat

tests :: TestEnv -> TestTree
tests env =
  testGroup
    "Stages 4-8"
    [ testGroup
        "Stage 4 - if, guards and case"
        [ normalFormIs env "10-2 qsort acceptance" "examples/qsort-explicit.hs" (nats [0, 1, 2, 3, 4]),
          strategiesAgree env "examples/qsort-explicit.hs",
          refSteps env "qsort" "examples/qsort-explicit.hs" 73,
          testCase "10-1 P3 split has the shape the spec's worked example requires" $ do
            trs <- compileOrFail env "examples/qsort-explicit.hs"
            -- the scrutinee moves to argument 1 and one symbol branches once
            case ruleFor "split" trs of
              Just (F _ as, F k bs) -> do
                assertBool "split relays to a fresh symbol" (take 6 k == "split#")
                assertEqual "the relay permutes the scrutinee to the front" (length as) (length bs)
              _ -> assertFailure "no split rule",
          testCase "10-1 P1 the generated rules pass checkTRS" $ do
            a <- artifactOf env "examples/qsort-explicit.hs"
            arViolations a @?= [],
          testGroup
            "10-3 demand order is only observable under leftmost-outermost"
            [ testCase "leftmost-outermost stops within the limit" $ do
                trs <- compileOrFail env "examples/demand.hs"
                assertNormalises "demand" (runMainWith leftmostOutermost loopLimit trs),
              testCase "the normal form is A" $ do
                trs <- compileOrFail env "examples/demand.hs"
                decode (runFinal (runMainWith leftmostOutermost loopLimit trs))
                  @?= VOther (F "A" []),
              testCase "leftmost-innermost diverges, being a strict strategy" $ do
                trs <- compileOrFail env "examples/demand.hs"
                assertBool
                  "innermost should hit the limit"
                  (not (runNormal (runMainWith leftmostInnermost loopLimit trs)))
            ]
        ],
      testGroup
        "Stage 2 - laziness regression (8-2)"
        [ testCase "False && loop is False without evaluating loop" $ do
            trs <- compileOrFail env "examples/shortcircuit.hs"
            let r = runMainWith leftmostOutermost loopLimit trs
            assertNormalises "shortcircuit" r
            decode (runFinal r) @?= VBool False,
          refSteps env "shortcircuit" "examples/shortcircuit.hs" 2,
          testCase "leftmost-innermost diverges on it" $ do
            trs <- compileOrFail env "examples/shortcircuit.hs"
            assertBool
              "innermost should hit the limit"
              (not (runNormal (runMainWith leftmostInnermost loopLimit trs)))
        ],
      testGroup
        "Stage 5 - where, let and lambda lifting"
        [ testCase "11-1 the inner and outer x become different variables" $ do
            trs <- compileOrFail env "examples/alpha.hs"
            let vs = trsVariables trs
            assertBool
              ("expected distinct renamed variables, got " ++ show vs)
              (length vs == length (dedup vs)),
          normalFormIs env "11-1 alpha conversion does not capture" "examples/alpha.hs" (VNat 1),
          testCase "11-2 P1 addAll lifts to exactly three rules" $ do
            trs <- compileOrFail env "examples/addall.hs"
            let go = [r | r@(F f _, _) <- trs, take 3 f == "go#"]
                addAll = [r | r@(F "addAll" _, _) <- trs]
            assertEqual "two rules for the lifted local, one relay" 2 (length go)
            assertEqual "one rule for addAll" 1 (length addAll),
          testCase "11-2 P1 the captured argument comes last in a saturated call" $ do
            trs <- compileOrFail env "examples/addall.hs"
            case ruleFor "addAll" trs of
              Just (_, F _ [V a, V b]) ->
                assertBool
                  ("expected (list, captured), got (" ++ a ++ ", " ++ b ++ ")")
                  (take 2 a == "xs" && take 1 b == "n")
              other -> assertFailure ("unexpected addAll rule: " ++ show other),
          normalFormIs env "11-2 addAll adds 1 to each element" "examples/addall.hs" (nats [1, 2]),
          testCase "11-2 P3 no UnboundRhsVar" $ do
            a <- artifactOf env "examples/addall.hs"
            arViolations a @?= []
        ],
      testGroup
        "Stage 6 - pattern match compilation"
        [ testCase "12-2 a wildcard expands to one rule per constructor" $ do
            trs <- compileOrFail env "examples/wildcard.hs"
            assertRulesAlpha
              "wildcard"
              (parseRules [] "f(A) -> Z  f(B) -> S(Z)  f(C) -> S(Z)")
              [r | r@(F "f" _, _) <- trs],
          testCase "12-2 P2 wildcard expansion leaves no RootOverlap" $ do
            a <- artifactOf env "examples/wildcard.hs"
            arViolations a @?= [],
          strategiesAgree env "examples/wildcard.hs",
          normalFormIs env "12-2 nested patterns swap the first two" "examples/firsttwo.hs" (nats [1, 0]),
          testCase "12-2 nested patterns leave no RootOverlap" $ do
            a <- artifactOf env "examples/firsttwo.hs"
            arViolations a @?= [],
          testCase "12-3 a non-exhaustive definition reaches patternMatchFail" $ do
            trs <- compileOrFail env "examples/headof.hs"
            let r = runMainWith leftmostOutermost defaultLimit trs
            assertNormalises "headof" r
            decode (runFinal r) @?= VFail,
          testCase "12-2 leq compiles to the spec's multi-column shape" $ do
            trs <- compileOrFail env "examples/qsort-explicit.hs"
            assertRulesAlpha
              "leq"
              ( parseRules
                  ["x", "y", "a"]
                  "leq(Z, y) -> True \
                  \ leq(S(x), y) -> leq#2(y, x) \
                  \ leq#2(Z, x) -> False \
                  \ leq#2(S(y), x) -> leq(x, y)"
              )
              [r | r@(F f _, _) <- trs, take 3 f == "leq"]
        ],
      testGroup
        "notation"
        [ normalFormIs env "the : operator works in patterns and expressions" "examples/colonlist.hs" (nats [3, 2, 1, 0]),
          testCase "x : xs desugars to the wired-in Cons" $ do
            trs <- compileOrFail env "examples/colonlist.hs"
            assertBool
              "no ':' symbol should survive into the TRS"
              (not (any ((== ":") . fst) (concatMap ruleSymbols trs))),
          testCase "[a, b] and a : b : Nil build the same term" $ do
            trs <- compileOrFail env "examples/colonlist.hs"
            case ruleFor "main" trs of
              Just (_, r) -> assertBool ("main should build two lists: " ++ show r) (True)
              Nothing -> assertFailure "no main rule",
          testCase "a Peano numeral prints as a decimal literal" $
            renderOneLine (prettyTermWith haskellNotation mempty (peanoTerm 3)) @?= "3",
          testCase "a Cons chain ending in Nil prints as a list" $
            renderOneLine (prettyTermWith haskellNotation mempty (listTerm (map peanoTerm [1, 2])))
              @?= "[1, 2]",
          testCase "a Cons chain ending in a variable prints with :" $
            renderOneLine
              (prettyTermWith haskellNotation mempty (F "Cons" [peanoTerm 1, V "xs"]))
              @?= "1 : xs",
          testCase "Nil prints as the empty list" $
            renderOneLine (prettyTermWith haskellNotation mempty (F "Nil" [])) @?= "[]",
          testCase "notation folding is display only, never term rewriting" $
            let t = listTerm (map peanoTerm [1, 2])
             in numeralOf haskellNotation t @?= Nothing,
          testCase "plainNotation folds nothing" $
            renderOneLine (prettyTermWith plainNotation mempty (peanoTerm 2))
              @?= "Succ(Succ(Zero))"
        ],
      testGroup
        "Stage 7 - higher order and defunctionalization"
        [ normalFormIs env "13-1 P1 the map example" "examples/map.hs" (nats [1, 2]),
          strategiesAgree env "examples/map.hs",
          refSteps env "map" "examples/map.hs" 13,
          testCase "13-1 the generated rules match the spec's shape" $ do
            trs <- compileOrFail env "examples/map.hs"
            assertRulesAlpha
              "apply"
              (parseRules ["x", "y"] "apply#(Clos#add#1(x), y) -> add(x, y)")
              [r | r@(F f _, _) <- trs, f == unVarName applyName],
          testCase "13-1 P3 checkTRS stays empty, so apply is orthogonal" $ do
            a <- artifactOf env "examples/map.hs"
            arViolations a @?= [],
          testCase "13-1 closures are constructors of one synthetic type" $ do
            trs <- compileOrFail env "examples/map.hs"
            let closures = [f | (f, _) <- concatMap ruleSymbols trs, "Clos#" `isPrefixOf` f]
            assertBool "a closure constructor should exist" (not (null closures))
            assertBool
              "no closure may be the root of a rule, they are constructors"
              (all (`notElem` closures) [f | (F f _, _) <- trs]),
          normalFormIs env "13-2 a lambda rides the lifting path" "examples/lambda-map.hs" (nats [2]),
          testCase "13-2 the lambda becomes a top-level function with a closure" $ do
            trs <- compileOrFail env "examples/lambda-map.hs"
            let syms = map fst (concatMap ruleSymbols trs)
            assertBool "a lifted lambda should exist" (any (elem '@') syms)
            assertBool "and a closure for it" (any ("Clos#" `isPrefixOf`) syms),
          normalFormIs env "13-1 P4 stepwise saturation" "examples/clos-chain.hs" (nats [2, 3]),
          testCase "13-1 P4 the closure set is upward closed" $ do
            trs <- compileOrFail env "examples/clos-chain.hs"
            let closures = [f | (f, _) <- concatMap ruleSymbols trs, "Clos#add3#" `isPrefixOf` f]
            assertBool
              ("both Clos#add3#1 and Clos#add3#2 are needed, got " ++ show closures)
              (("Clos#add3#1" `elem` closures) && ("Clos#add3#2" `elem` closures)),
          normalFormIs env "13-3 foldr over a two-argument lambda" "examples/Prelude/HO.hs" (nats [0, 1]),
          testCase "13-1 P2 a program with no partial application costs nothing" $ do
            let noApply rel = do
                  trs <- compileOrFail env rel
                  a <- artifactOf env rel
                  assertBool
                    (rel ++ " should generate no apply rules")
                    (not (any ((== unVarName applyName) . fst) (concatMap ruleSymbols trs)))
                  assertBool
                    (rel ++ " should not even declare apply")
                    (not (Map.member (unVarName applyName) (arSig a)))
            mapM_ noApply
              [ "examples/Prelude/Base.hs",
                "examples/qsort.hs",
                "examples/simple.hs",
                "examples/addall.hs"
              ]
        ],
      testGroup
        "Stage 10 - self consistency (16-1)"
        [ testCase "the core language compiles with every sugar layer off" $ do
            src <- readExample env "test/data/stage10/core.hs"
            case compileWith coreOptions "core.hs" src of
              Left e -> assertFailure ("core-only rejected the core language: " ++ show e)
              Right _ -> pure (),
          testCase "and evaluates to the same thing with the layers on" $ do
            src <- readExample env "test/data/stage10/core.hs"
            let nf' opts = do
                  trs <- either (fail . show) pure (compileWith opts "core.hs" src)
                  pure (decode (runFinal (runMainWith leftmostOutermost defaultLimit trs)))
            a <- nf' coreOptions
            b <- nf' defaultOptions
            (a, b) @?= (VNat 2, VNat 2),
          testCase "with the layers off, each one is named as the gap" $ do
            let expect rel want = do
                  src <- readExample env rel
                  case compileWith coreOptions rel src of
                    Left (Unsupported f) -> f @?= want
                    Left e -> assertFailure (rel ++ ": wrong error " ++ show e)
                    Right _ -> assertFailure (rel ++ ": expected the layer to be off")
            expect "examples/qsort-explicit.hs" FGuards
            expect "examples/simple.hs" FNumericLiteral
            expect "examples/wildcard.hs" FNumericLiteral,
          testCase "the where layer is named when it is the only sugar used" $ do
            src <- readExample env "test/data/stage10/where.hs"
            case compileWith coreOptions "where.hs" src of
              Left (Unsupported f) -> f @?= FWhereClause
              Left e -> assertFailure ("wrong error: " ++ show e)
              Right _ -> assertFailure "expected the where layer to be off",
          testCase "6-1 P2 removing data Bool makes the guard layer complain" $ do
            src <- readExample env "examples/qsort-explicit.hs"
            let without = unlines [l | l <- lines src, take 9 l /= "data Bool"]
            case compileModule "qsort.hs" without of
              Left (MissingKnownName k _ _) -> k @?= KnTrue
              Left e -> assertFailure ("wrong error: " ++ show e)
              Right _ -> assertFailure "expected the guard layer to need True"
        ],
      testGroup
        "Stage 8 - literals"
        [ normalFormIs env "14-3 a literal pattern needs no equality test" "examples/litpat.hs" (nats [3, 2, 1]),
          testCase "14-2 sucB three times over One is four" $ do
            trs <- compileOrFail env "examples/Prelude/Num.hs"
            let r = runMainWith leftmostOutermost defaultLimit trs
            assertNormalises "Num" r
            decode (runFinal r) @?= VBin 4,
          refSteps env "Prelude/Num.hs" "examples/Prelude/Num.hs" 5,
          testCase "14-1 P2 binary addB agrees with integer addition" $ do
            trs <- compileOrFail env "examples/Prelude/Num.hs"
            let bad =
                  [ (a, b)
                  | a <- [1 .. 12 :: Integer],
                    b <- [1 .. 12 :: Integer],
                    let t = F "addB" [binTerm a, binTerm b],
                    decode (runFinal (runWith leftmostOutermost defaultLimit trs t)) /= VBin (a + b)
                  ]
            bad @?= [],
          testCase "14-1 P1 a binary numeral is logarithmic, a Peano one linear" $ do
            map (termSize . binTerm) [1, 2, 4, 8, 16] @?= [1, 2, 3, 4, 5]
            map (termSize . peanoTerm) [0, 1, 2, 3] @?= [1, 2, 3, 4],
          testCase "14-3 the literal pattern produces no RootOverlap" $ do
            a <- artifactOf env "examples/litpat.hs"
            arViolations a @?= [],
          normalFormIs env "sugar-free arithmetic still works" "examples/simple.hs" (VNat 3),
          testCase "14-1 P3 the sugared and spelled-out qsort agree" $ do
            sugared <- compileOrFail env "examples/qsort.hs"
            plain <- compileOrFail env "examples/qsort-explicit.hs"
            let nf' trs = decode (runFinal (runMainWith leftmostOutermost defaultLimit trs))
            nf' sugared @?= nf' plain
        ]
    ]
  where
    dedup = foldr (\x xs -> if x `elem` xs then xs else x : xs) []
    coreOptions = defaultOptions {optLayers = coreOnly}

ruleSymbols :: Rule -> [(String, Int)]
ruleSymbols (l, r) = symbolOccurrences l ++ symbolOccurrences r

artifactOf :: TestEnv -> FilePath -> IO Artifact
artifactOf env rel = do
  src <- readExample env rel
  either (\e -> fail (rel ++ ": " ++ show e)) pure (compileArtifact defaultOptions rel src)
