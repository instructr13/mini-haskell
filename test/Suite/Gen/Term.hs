module Suite.Gen.Term
  ( SmallTerm (..),
    GroundTerm (..),
    LinearPat (..),
    SubstOf (..),
    SmallTRS (..),
    sigFuns,
    sigVars,
    genTerm,
    genLinear,
  )
where

import Test.QuickCheck
import TRS

sigFuns :: [(String, Int)]
sigFuns = [("0", 0), ("s", 1), ("nil", 0), ("cons", 2), ("f", 2), ("g", 1)]

sigVars :: [String]
sigVars = ["x", "y", "z"]

genTerm :: Bool -> Int -> Gen Term
genTerm allowVars d
  | d <= 0 = leaf
  | otherwise =
      frequency
        [ (1, leaf),
          (3, node)
        ]
  where
    leaf
      | allowVars =
          frequency [(1, V <$> elements sigVars), (1, pure (F "0" []))]
      | otherwise = elements [F f [] | (f, 0) <- sigFuns]
    node = do
      (f, n) <- elements sigFuns
      F f <$> vectorOf n (genTerm allowVars (d - 1))

newtype SmallTerm = SmallTerm Term

instance Show SmallTerm where show (SmallTerm t) = show t

instance Arbitrary SmallTerm where
  arbitrary = SmallTerm <$> sized (\n -> genTerm True (min 3 (1 + n `div` 4)))
  shrink (SmallTerm t) = map SmallTerm (shrinkTerm t)

newtype GroundTerm = GroundTerm Term

instance Show GroundTerm where show (GroundTerm t) = show t

instance Arbitrary GroundTerm where
  arbitrary = GroundTerm <$> sized (\n -> genTerm False (min 3 (1 + n `div` 4)))
  shrink (GroundTerm t) = [GroundTerm u | u <- shrinkTerm t, null (variables u)]

shrinkTerm :: Term -> [Term]
shrinkTerm (V _) = []
shrinkTerm (F _ ts) = ts

genLinear :: Int -> Gen Term
genLinear d = do
  t <- genTerm True d
  pure (relabel t)
  where
    relabel u = fst (go u (0 :: Int))
    go (V _) i = (V ("v" ++ show i), i + 1)
    go (F f ts) i =
      let step (acc, j) x = let (x', j') = go x j in (acc ++ [x'], j')
          (ts', i') = foldl step ([], i) ts
       in (F f ts', i')

newtype LinearPat = LinearPat Term

instance Show LinearPat where show (LinearPat t) = show t

instance Arbitrary LinearPat where
  arbitrary = LinearPat <$> sized (\n -> genLinear (min 3 (1 + n `div` 4)))

newtype SubstOf = SubstOf Subst

instance Show SubstOf where show (SubstOf s) = show s

instance Arbitrary SubstOf where
  arbitrary = do
    xs <- sublistOf sigVars
    ts <- mapM (const (genTerm True 2)) xs
    pure (SubstOf (zip xs ts))
  shrink (SubstOf s) = [SubstOf s' | s' <- shrinkList (const []) s]

data SmallTRS = SmallTRS TRS

instance Show SmallTRS where show (SmallTRS t) = showTRS t

instance Arbitrary SmallTRS where
  arbitrary = do
    n <- choose (1, 4)
    SmallTRS <$> vectorOf n rule
    where
      rule = do
        (f, k) <- elements [(f', k') | (f', k') <- sigFuns, k' > 0]
        args <- vectorOf k (genTerm True 1)
        let l = F f args
        r <- genTerm True 1
        pure (l, restrict (variables l) r)
      restrict vs t = case vs of
        [] -> ground t
        _ -> keep vs t
      keep vs (V x) = if x `elem` vs then V x else V (headOr x vs)
      keep vs (F f ts) = F f (map (keep vs) ts)
      ground (V _) = F "0" []
      ground (F f ts) = F f (map ground ts)
      headOr d [] = d
      headOr _ (v : _) = v
