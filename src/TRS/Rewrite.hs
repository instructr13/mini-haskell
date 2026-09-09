{-# LANGUAGE BangPatterns #-}

module TRS.Rewrite (module TRS.Rewrite) where

import Data.List (isPrefixOf, nubBy, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import TRS
import TRS.Match
import Term

type Strategy = IndexedTRS -> Term -> Maybe Term

newtype IndexedTRS = IndexedTRS (Map String [Rule])

indexTRS :: TRS -> IndexedTRS
indexTRS trs = IndexedTRS (Map.map (sortOn (Down . ruleArity)) buckets)
  where
    buckets = Map.fromListWith (flip (++)) [(f, [rule]) | rule@(F f _, _) <- trs]

rulesFor :: IndexedTRS -> Term -> [Rule]
rulesFor (IndexedTRS m) (F f _) = Map.findWithDefault [] f m
rulesFor _ (V _) = []

-- {t | s ->_R t} = {s[r sigma]_p | ∃ p ∈ Pos(s). ∃ l -> r ∈ R. ∃ sigma which satisfies l sigma = s|_p}
reducts :: IndexedTRS -> Term -> [(Position, Term)]
reducts trs s =
  [ (p, substitute r sigma `applyTo` rest)
  | p <- positions s,
    Just u <- [subTermAt s p],
    (l, r) <- rulesFor trs u,
    Just (sigma, rest) <- [matchRoot l u]
  ]

-- rewrite R t = Just u, if t ->_R u for some term u
-- rewrite R t = Nothing, otherwise
rewrite :: Strategy
rewrite trs s =
  case reducts trs s of
    [] -> Nothing
    rs -> Just (foldl' f s (nubBy encloses rs))
  where
    encloses :: (Eq a) => ([a], b) -> ([a], c) -> Bool
    encloses (p, _) (q, _) = p `isPrefixOf` q

    f :: Term -> ([Int], Term) -> Term
    f acc (p, t) = case replace acc t p of
      Just acc' -> acc'
      Nothing -> acc

-- nf R t = u if t ->_R ... ->_R u for some normal form u
nfWith :: Strategy -> IndexedTRS -> Term -> Term
nfWith f trs t0 = go t0
  where
    go !t = case f trs t of
      Just t' -> go t'
      _ -> t

-- nf with the limit.
nfBounded :: Int -> TRS -> Term -> Maybe Term
nfBounded limit trs0 = go limit
  where
    trs = indexTRS trs0
    go !k t
      | k <= 0 = Nothing
      | otherwise = case rewrite trs t of
          Just t' -> go (k - 1) t'
          Nothing -> Just t

-- The index is built once here, not once per rewrite step.
nf :: TRS -> Term -> Term
nf = nfWith rewrite . indexTRS
