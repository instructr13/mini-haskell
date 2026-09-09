{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module TRS (module TRS) where

import Data.List (intercalate, isPrefixOf, nub, nubBy, (!?))
import Data.Maybe
import Prettyprinter

data Term = V String | F String [Term] deriving (Eq)

instance Pretty Term where
  pretty = go False
    where
      go :: Bool -> Term -> Doc ann
      go _ t | (h, []) <- flatten t = pretty h
      go p t
        | (h, args) <- flatten t =
            parensIf p (hang 2 (sep (pretty h : map (go True) args)))

      parensIf :: Bool -> Doc ann -> Doc ann
      parensIf b = if b then parens else id

      flatten :: Term -> (String, [Term])
      flatten = go' []
        where
          go' acc (V x) = (x, acc)
          go' acc (F f bs) = (f, bs ++ acc)

type Position = [Int]

type Subst = [(String, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

type Strategy = TRS -> Term -> Maybe Term

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")

-- Special function for function application operator (juxtaposition notation)
appName :: String
appName = "@"

-- is f :@ x = @(f, x) ?
pattern (:@) :: Term -> Term -> Term
pattern f :@ x <- F "@" [f, x]
  where
    f :@ x = F appName [f, x]

infixl 9 :@

apps :: Term -> [Term] -> Term
apps = foldl (:@)

-- @(@(f,a),b) ==> (f, [a,b])
spine :: Term -> (Term, [Term])
spine = go []
  where
    go acc (f :@ x) = go (x : acc) f
    go acc t = (t, acc)

-- Rename every variable apart by appending a suffix.
-- renameTerm "'" (F "f" [V "x", V "y"]) = F "f" [V "x'", V "y'"]
renameTerm :: String -> Term -> Term
renameTerm suffix (V x) = V (x ++ suffix)
renameTerm suffix (F f ts) = F f [renameTerm suffix t | t <- ts]

-- D(R) = {root(l) | l -> r ∈ R}
definedSymbols :: TRS -> [String]
definedSymbols trs = nub [f | (F f _, _) <- trs]

-- Pos(t)
-- positions (F "add" []) = [[]]
-- positions (F "add" [V "x"]) = [[], [0]]
--                             = [] : [[0]]
--                             = [] : [0 : p | p <- positions (V "x")]
-- positions (F "add" [V "x", V "y"]) = [[], [0], [1]]
--                                    = [] : [[0]] ++ [[1]]
--                                    = [] : [[0]] ++ [p + 1 : ps' | (p : ps') <- [[], [0]]]
--                                    = [] : [0 : p | p <- positions (V "x")]
--                                      ++ [p + 1 : ps' | (p : ps') <- positions (F "add" [V "y"])]
-- positions (F "add" [V "x", V "y", V "z"]) = [[], [0], [1], [2]]
-- positions (F "add" [F "add" [V "x", V "y"], V "y", V "z"]) = [[], [0], [0, 0], [0, 1], [1], [2]]
positions :: Term -> [Position]
positions (V _) = [[]] -- {ε}
positions (F _ []) = [[]] -- {ε}
positions (F _ ts) = [[]] ++ [i : j | i <- [0 .. length ts - 1], j <- positions (ts !! i)]

-- subTermAt t p = t|_p
-- subTermAt (F "add" [V "x", V "y"]) [] = F "add" [V "x", V "y"]
-- subTermAt (F "add" [V "x", V "y"]) [0] = V "x"
-- subTermAt (F "add" [V "x", V "y"]) [1] = V "y"
-- subTermAt (F "add" [(F "add" [V "x", V "y"]), V "y"]) [0, 0] = V "x"
subTermAt :: Term -> Position -> Maybe Term
subTermAt t [] = Just t
subTermAt (V _) _ = error "subTermAt: cannot use sub-position to a variable"
subTermAt (F _ ts) (i : ps) = case ts !? i of
  Just u -> subTermAt u ps
  _ -> Nothing

-- replace t u p = t[u]_p
-- replace (F "add" [V "x", V "y"]) (F "0" []) [0] = F "add" [(F "0" []), V "y"]
-- replace (F "add" [(F "add" [V "x", V "y"]), V "y"]) (F "0" []) [0, 0]
--   = F "add" [F "add" [F "0" [], V "y"], V "y"]
replace :: Term -> Term -> Position -> Maybe Term
replace _ u [] = Just u
replace (V _) _ _ = error "replace: cannot use sub-position to a variable"
replace (F f ts) u (i : ps)
  | (pre, c : post) <- splitAt i ts = case replace c u ps of
      Just c' -> Just (F f (pre ++ c' : post))
      _ -> Nothing
  | otherwise = Nothing

-- Var(t)
variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]

-- substitute t sigma = t sigma
substitute :: Term -> Subst -> Term
substitute (V x) sigma = fromMaybe (V x) (lookup x sigma)
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- theta = compose sigma tau
--       = {x ↦ t tau | (x ↦ t) ∈ sigma} ∪ {y ↦ t | (y ↦ t) ∈ tau, y ∉ Dom(sigma)}
-- substitute t (compose sigma tau) = substitute (substitute t sigma) tau
compose :: Subst -> Subst -> Subst
compose sigma tau = [(x, substitute t tau) | (x, t) <- sigma] ++ [b | b@(x, _) <- tau, x `notElem` dom]
  where
    dom = map fst sigma

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
match s0 t0 = go [] [(s0, t0)]
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

occurs :: String -> Term -> Bool
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

-- {t | s ->_R t} = {s[r sigma]_p | ∃ p ∈ Pos(s). ∃ l -> r ∈ R. ∃ sigma which satisfies l sigma = s|_p}
reducts :: TRS -> Term -> [(Position, Term)]
reducts trs s =
  [(p, substitute r sigma) | p <- positions s, Just u <- [subTermAt s p], (l, r) <- trs, Just sigma <- [match l u]]

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
nfWith :: Strategy -> TRS -> Term -> Term
nfWith f trs t0 = go t0
  where
    go !t = case f trs t of
      Just t' -> go t'
      _ -> t

nf :: TRS -> Term -> Term
nf trs t = nfWith rewrite trs t

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
