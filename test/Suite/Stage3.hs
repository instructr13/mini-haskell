module Suite.Stage3 (tests) where

import Control.Exception (evaluate, try)
import qualified Control.Exception as E
import HS.Error (CompileError (..))
import HS.Layout
import HS.Lexer
import HS.Parser (parseTokens)
import Suite.Gen.Layout
import System.Timeout (timeout)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

lexOrFail :: String -> [PosToken]
lexOrFail src = case lexHS "t.hs" src of
  Left e -> error (prettyLexError e)
  Right ts -> ts

-- (P1) layout inserts only: deleting the virtual tokens gives the input back,
-- positions included.
insertionOnly :: [PosToken] -> Bool
insertionOnly ts = filter (not . isVirtual . ptTok) (layout ts) == ts

-- (P2) the virtual braces are balanced.
balanced :: [PosToken] -> Bool
balanced ts = go (0 :: Int) (map ptTok (layout ts))
  where
    go d [] = d == 0
    go d (TVLBrace : r) = go (d + 1) r
    go d (TVRBrace : r) = d > 0 && go (d - 1) r
    go d (_ : r) = go d r

-- (P4) two spellings of the same thing agree modulo virtual/explicit.
normalised :: String -> [Token]
normalised = map (unvirtual . ptTok) . layout . lexOrFail

-- The three sources the spec requires, as (name, explicit, layout) triples.
pairs :: [(String, String, String)]
pairs =
  [ ( "where block",
      "f x = a where { a = x }\n",
      "f x = a\n  where a = x\n"
    ),
    ( "of block",
      "f x = case x of { Nil -> Z ; Cons y ys -> y }\n",
      "f x = case x of\n  Nil       -> Z\n  Cons y ys -> y\n"
    ),
    ( "let ... in",
      "g = let { x = Z } in x\n",
      "g = let x = Z in x\n"
    )
  ]

tests :: TestTree
tests =
  testGroup
    "Stage 3 - layout"
    [ testGroup
        "the required sources agree modulo virtual/explicit"
        [ testCase name $ normalised a @?= normalised b
        | (name, a, b) <- pairs
        ],
      testCase "layout of the empty input is empty" $
        layout [] @?= [],
      testCase "an explicitly braced module body is left alone" $
        take 1 (map ptTok (normalisedTokens "{ a = Z ; b = Z }\n"))
          @?= [TSpecial '{'],
      testCase "a module body with no braces gets an implicit block" $
        take 1 (map ptTok (layout (lexOrFail "a = Z\nb = Z\n")))
          @?= [TVLBrace],
      testCase "declarations at the same column are separated" $
        countTok TVSemi (layout (lexOrFail "a = Z\nb = Z\nc = Z\n")) @?= 2,
      testCase "a continuation line is not a new declaration" $
        countTok TVSemi (layout (lexOrFail "a = f\n  Z\nb = Z\n")) @?= 1,
      testCase "in closes exactly one implicit block" $
        countTok TVRBrace (layout (lexOrFail "g = let x = Z in x\n")) @?= 2,
      testCase "an explicit let block is not closed twice by in" $
        countTok TVRBrace (layout (lexOrFail "g = let { x = Z } in x\n")) @?= 1,
      testGroup
        "9-2 termination on bad indentation"
        [ testCase "layout terminates on a column that decreases then increases" $ do
            src <- readFile "test/data/stage3/bad-indent.hs"
            r <- timeout (5 * 1000 * 1000) (evaluate (length (layout (lexOrFail src))))
            case r of
              Nothing -> assertFailure "layout did not terminate"
              Just _ -> pure (),
          testCase "the parser rejects it with a positioned error" $ do
            src <- readFile "test/data/stage3/bad-indent.hs"
            case parseTokens "bad-indent.hs" src (layout (lexOrFail src)) of
              Left (SyntaxError _ _) -> pure ()
              Left e -> assertFailure ("expected SyntaxError, got " ++ show e)
              Right _ -> assertFailure "expected a syntax error"
        ,  testCase "a token left of the outermost context reports an underflow" $ do
            src <- readFile "test/data/stage3/underflow.hs"
            let (_, evs) = layoutTrace (lexOrFail src)
            assertBool
              ("expected an Underflow event, got " ++ show evs)
              (any isUnderflow evs)
        ],
      testProperty "P1 layout inserts only, on real sources" $
        \(LayoutSource ds) ->
          conjoin
            [ property (insertionOnly (lexOrFail (renderStyled s ds)))
            | s <- [Explicit, Layout]
            ],
      testProperty "P2 virtual braces are balanced, on real sources" $
        \(LayoutSource ds) ->
          conjoin
            [ property (balanced (lexOrFail (renderStyled s ds)))
            | s <- [Explicit, Layout]
            ],
      testProperty "P4 explicit and layout spellings agree" $
        \(LayoutSource ds) ->
          map (unvirtual . ptTok) (layout (lexOrFail (renderStyled Explicit ds)))
            === map (unvirtual . ptTok) (layout (lexOrFail (renderStyled Layout ds))),
      testProperty "P1 layout inserts only, on adversarial token streams" $
        \(TokenSoup ts) -> property (insertionOnly ts),
      testProperty "P2 virtual braces are balanced, on adversarial streams" $
        \(TokenSoup ts) -> property (balanced ts),
      testProperty "P3 layout terminates on adversarial streams" $
        \(TokenSoup ts) -> ioProperty $ do
          r <- try (timeout (2 * 1000 * 1000) (evaluate (length (layout ts)))) :: IO (Either E.SomeException (Maybe Int))
          pure $ case r of
            Right (Just _) -> True
            Right Nothing -> False
            Left _ -> False
    ]
  where
    normalisedTokens = layout . lexOrFail
    countTok t = length . filter ((== t) . ptTok)
    isUnderflow Underflow {} = True
    isUnderflow _ = False
