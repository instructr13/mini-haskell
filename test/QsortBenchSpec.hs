{-# LANGUAGE BangPatterns #-}

-- Step-count benchmark for qsort.
module QsortBenchSpec (spec) where

import Control.Exception (evaluate)
import Data.List (intercalate, sort, sortOn)
import HS.Pretty (prettyTerm)
import Numeric (showFFloat)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.String (renderString)
import Source (compileSrc)
import TRS (TRS)
import TRS.Rewrite (indexTRS, rewrite)
import Term
import Test.Hspec

stepLimit :: Int
stepLimit = 50000

qsortRules :: [String]
qsortRules =
  [ "qsort []                   -> [] ;",
    "qsort (x : xs)             -> split x xs [] [] ;",
    "split w []           ys zs -> qsort ys ++ (w : qsort zs) ;",
    "split w (x : xs)     ys zs -> ifSplit (w <= x) w x xs ys zs ;",
    "ifSplit True  w x xs ys zs -> split w xs ys (x : zs) ;",
    "ifSplit False w x xs ys zs -> split w xs (x : ys) zs ;"
  ]

benchFile :: FilePath
benchFile = "<qsort-bench>.trst"

listLiteral :: [Int] -> String
listLiteral xs = "[" ++ intercalate ", " (map show xs) ++ "]"

program :: [Int] -> String
program xs =
  unlines $
    ["(RULES"]
      ++ map ("  " ++) qsortRules
      ++ ["  main -> qsort " ++ listLiteral xs ++ " ;", ")"]

data Bench = Bench
  { benchSize :: Int,
    -- | Nothing when stepLimit was reached.
    benchSteps :: Maybe Int,
    benchResult :: String,
    benchExpected :: String
  }

-- Count the rewrite steps to the normal form, giving up after stepLimit.
-- The index is built once, as TRS.Rewrite.nf does.
countSteps :: TRS -> Term -> (Maybe Int, Term)
countSteps trs0 = go 0
  where
    trs = indexTRS trs0
    go !k t
      | k >= stepLimit = (Nothing, t)
      | otherwise = case rewrite trs t of
          Just t' -> go (k + 1) t'
          Nothing -> (Just k, t)

render :: Term -> String
render t = renderString (layoutPretty defaultLayoutOptions (prettyTerm t))

measure :: [Int] -> Bench
measure xs = case compileSrc benchFile (program xs) of
  Left _ -> failed "<compile error>"
  Right trs -> case lookup (F "main" []) trs of
    Nothing -> failed "<no main>"
    Just t -> let (steps, u) = countSteps trs t in Bench n steps (render u) expected
  where
    n = length xs
    expected = listLiteral (sort xs)
    failed msg = Bench n Nothing msg expected

shuffled :: Int -> [Int]
shuffled n = map snd (sortOn fst (zip keys [0 .. n - 1]))
  where
    keys = take n (iterate lcg 42)
    lcg s = (s * 1103515245 + 12345) `mod` 2147483648

ascending :: Int -> [Int]
ascending n = [0 .. n - 1]

padLeft :: Int -> String -> String
padLeft w s = replicate (w - length s) ' ' ++ s

-- The measurement is the test description, so the table shows up in the report.
label :: Bench -> String
label b =
  "n = "
    ++ padLeft 3 (show (benchSize b))
    ++ "   steps = "
    ++ padLeft 6 steps
    ++ "   steps/n^2 = "
    ++ ratio
  where
    steps = maybe ("> " ++ show stepLimit) show (benchSteps b)
    ratio = case (benchSteps b, benchSize b) of
      (Just s, n)
        | n > 0 ->
            showFFloat (Just 2) (fromIntegral s / fromIntegral (n * n) :: Double) ""
      _ -> "-"

row :: Bench -> Spec
row b = it (label b) $ case benchSteps b of
  Nothing ->
    expectationFailure ("no normal form within " ++ show stepLimit ++ " steps")
  Just _ -> benchResult b `shouldBe` benchExpected b

-- Measure up front so the counts are available as descriptions.
table :: (Int -> [Int]) -> [Int] -> Spec
table gen sizes = do
  rows <- runIO (mapM (force . measure . gen) sizes)
  mapM_ row rows
  where
    force b = do
      _ <- evaluate (benchSteps b)
      _ <- evaluate (length (benchResult b))
      pure b

spec :: Spec
spec = describe "qsort step count" $ do
  describe "pseudo-random input" $
    table shuffled [0, 1, 2, 4, 8, 16, 24, 32, 48]

  describe "sorted input (worst case)" $
    table ascending [4, 8, 16, 32]
