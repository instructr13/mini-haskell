module TRS.Match (module TRS.Match) where

import TRS
import Term

-- Rule I: {f(s_1, ..., s_n) ↦ f(t_1, ..., t_n)} ∪ S ==> {s_1 ↦ t_1, ..., s_n ↦ t_n} ∪ S
-- Rule II: {f(s_1, ..., s_n) ↦ g(t_1, ..., t_n)} ∪ S ==> ⊥ if f /= g, do not decompose
-- Includes Rule III: {f(s_1, ..., s_n) ↦ x} ∪ S ==> ⊥, do not decompose
decompose :: Term -> Term -> Maybe [(Term, Term)]
decompose (F f ts) (F g us)
  | f == g && length ts == length us = Just (zip ts us)
decompose _ _ = Nothing

-- match s t = Just sigma, if s sigma = t for some sigma
-- match s t = Nothing, otherwise
-- match (F "add" [V "x", (F "s" [(F "add" [V "y", V "z"])])]) (F "add" [(F "s" [V "y"]), (F "s" [(F "add" [(F "add" [V "x", (F "0" [])]), V "z"])])])
--   = Just [("x", F "s" [V "y"]), ("y", F "add" [V "x", (F "0" [])]), ("z", V "z")]
-- match (F "add" [(F "s" [V "x"]), (F "add" [V "x", V "y"])]) (F "add" [(F "s" [(F "add" [(F "0" []), V "x"])]), (F "add" [(F "add" [(F "0" []), (F "0" [])]), V "x"])])
--   = Nothing
match :: Term -> Term -> Maybe Subst
match s t = matchAll [(s, t)]

-- Solve a whole worklist of (pattern, term) pairs with one substitution.
matchAll :: [(Term, Term)] -> Maybe Subst
matchAll = go []
  where
    go :: Subst -> [(Term, Term)] -> Maybe Subst
    go sigma [] = Just sigma
    -- Rule IV: {x ↦ t} ∪ S ==> ⊥ if x ↦ t' ∈ S with t /= t'
    go sigma ((V x, t) : ts) =
      case lookup x sigma of
        Just t' | t /= t' -> Nothing
        Just _ -> go sigma ts
        _ -> go ((x, t) : sigma) ts
    go sigma ((s, t) : ts) = case decompose s t of
      Just new -> go sigma (new ++ ts)
      _ -> Nothing

-- Match a rule's left-hand side at the root of a term, letting the term carry
-- more arguments than the pattern (over-application).
-- matchRoot (F "id" [V "x"]) (F "id" [a, b]) = Just ([("x", a)], [b])
-- matchRoot (F "add1" [V "x"]) (F "add1" []) = Nothing
matchRoot :: Term -> Term -> Maybe (Subst, [Term])
matchRoot (F f as) (F g bs)
  | f == g,
    (bs', rest) <- splitAt (length as) bs,
    length bs' == length as =
      fmap (\sigma -> (sigma, rest)) (matchAll (zip as bs'))
matchRoot _ _ = Nothing

occurs :: Name -> Term -> Bool
occurs x (V y) = x == y
occurs x (F _ ts) = any (occurs x) ts

-- Unification
-- unify (F "f" [V "x", F "a" []]) (F "f" [F "b" [], V "y"]) = Just [("y",a),("x",b)]
-- unify (V "x") (F "f" [V "x"]) = Nothing
unify :: Term -> Term -> Maybe Subst
unify s0 t0 = go [] [(s0, t0)]
  where
    go :: Subst -> [(Term, Term)] -> Maybe Subst
    go sigma [] = Just sigma
    go sigma ((V x, t) : ts)
      | V y <- t, x == y = go sigma ts
      | occurs x t = Nothing -- x ∈ Var(t) ==> ⊥
      | otherwise = go sigma' ts'
      where
        ts' = [(substitute t1 [(x, t)], substitute t2 [(x, t)]) | (t1, t2) <- ts]
        sigma' = (x, t) : [(s', substitute t' [(x, t)]) | (s', t') <- sigma]
    -- Replace of Rule III: {f(s_1, ..., s_n) ↦ x} ∪ S
    --                  ==> {x ↦ f(s_1, ..., s_n)} ∪ S
    -- to allow bidirectional matching
    go sigma ((t@(F _ _), V x) : ts) = go sigma ((V x, t) : ts)
    -- Rule I, II
    go sigma ((s, t) : ts) = case decompose s t of
      Just new -> go sigma (new ++ ts)
      _ -> Nothing
