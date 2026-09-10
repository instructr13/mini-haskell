module TRS.Rewrite (module TRS.Rewrite) where

import Data.List (mapAccumL, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import TRS
import TRS.Match
import Term

type IndexedTRS = Map Name [Rule]

indexTRS :: TRS -> IndexedTRS
indexTRS trs = Map.map (sortOn (Down . ruleArity)) buckets
  where
    buckets = Map.fromListWith (flip (++)) [(f, [rule]) | rule@(F f _, _) <- trs]

rulesFor :: IndexedTRS -> Term -> [Rule]
rulesFor m (F f _) = Map.findWithDefault [] f m
rulesFor _ (V _) = []

-- Get accessible functions using the advanced fixed-point theorem for sets
accessibleFunctions :: IndexedTRS -> Term -> Set Name
accessibleFunctions trs t = go Set.empty (termSymbols t [])
  where
    succs g gs = foldr step gs (Map.findWithDefault [] g trs)
    step (l, r) acc = termSymbols l (termSymbols r acc)

    go :: Set Name -> [Name] -> Set Name
    go seen [] = seen
    go seen frontier = go seen' [g | g <- newSyms, g `Set.notMember` seen']
      where
        newSyms = foldr (\g acc -> succs g acc) [] frontier
        seen' = foldr Set.insert seen frontier

pruneIndexedTRS :: IndexedTRS -> Term -> IndexedTRS
pruneIndexedTRS itrs t = Map.restrictKeys itrs (accessibleFunctions itrs t)

prepareTRS :: TRS -> Term -> IndexedTRS
prepareTRS = pruneIndexedTRS . indexTRS

-- t * sigma, the normal form of t sigma:
--
--   x * sigma                = x sigma
--   f(t_1, ..., t_n) * sigma = r * tau   if t' = l tau for some l -> r in R
--                            = t'        otherwise
--     where t' = f(t_1 * sigma, ..., t_n * sigma)
nfIndexedSteps :: Int -> IndexedTRS -> Term -> (Bool, Int, Term)
nfIndexedSteps limit trs t0 = (steps <= limit, min steps limit, u)
  where
    (steps, u) = go 0 [] [] t0

    go :: Int -> Subst -> [Term] -> Term -> (Int, Term)
    -- no root calls required for rest = []
    go n sigma [] (V x) = (n, substitute (V x) sigma)
    -- x * sigma = x sigma
    go n sigma rest (V x) = root n (substitute (V x) sigma `applyTo` rest)
    -- (a @ b) * sigma = (a * sigma) @ (b * sigma)
    go n sigma rest (a :@ b) = root n'' (mkApp a' b' `applyTo` rest)
      where
        (n', a') = go n sigma [] a
        (n'', b') = go n' sigma [] b
    go n sigma rest (F f ts) = root n' (F f ts' `applyTo` rest)
      where
        goAccum :: Int -> Term -> (Int, Term)
        goAccum k t = go k sigma [] t

        (n', ts') = mapAccumL goAccum n ts

    root :: Int -> Term -> (Int, Term)
    root n t =
      case [(r, tau, rest) | (l, r) <- rulesFor trs t, Just (tau, rest) <- [matchRoot l t]] of
        [] -> (n, t)
        ((r, tau, rest) : _)
          | n >= limit -> (limit + 1, t) -- Limit exceeded
          | otherwise -> go (n + 1) tau rest r -- r * tau

nfBoundedSteps :: Int -> TRS -> Term -> (Bool, Int, Term)
nfBoundedSteps limit trs t = nfIndexedSteps limit (prepareTRS trs t) t

nfIndexed :: Int -> IndexedTRS -> Term -> (Bool, Term)
nfIndexed limit trs t = (success, finalTerm)
  where
    (success, _, finalTerm) = nfIndexedSteps limit trs t

nfBounded :: Int -> TRS -> Term -> (Bool, Term)
nfBounded limit trs t = nfIndexed limit (prepareTRS trs t) t

nf :: TRS -> Term -> Term
nf trs t = snd (nfBounded maxBound trs t)
