{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module TRS (module TRS) where

import Data.List (nub, (!?))
import Data.Maybe
import Term

type Position = [Int]

type Subst = [(Name, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

-- Special function for function application operator (juxtaposition notation).
appName :: Name
appName = "@"

-- Is f :@ x = @(f, x) ?
pattern (:@) :: Term -> Term -> Term
pattern f :@ x <- F ((== appName) -> True) [f, x]
  where
    f :@ x = F appName [f, x]

infixl 9 :@

-- Smart application.
-- mkApp (F "add1" []) (F "0" []) = F "add1" [F "0" []]
-- mkApp (V "f") (V "x") = V "f" :@ V "x"
mkApp :: Term -> Term -> Term
mkApp (F f ts) x | f /= appName = F f (ts ++ [x])
mkApp f x = f :@ x

-- Apply more arguments to a term, left to right.
-- (t `applyTo` as) `applyTo` bs = t `applyTo` (as ++ bs)
applyTo :: Term -> [Term] -> Term
applyTo = foldl mkApp

-- Inverse of applyTo for an unresolved application.
-- appSpine (V "f" :@ a :@ b) = Just (V "f", [a, b])
-- appSpine (F "add1" [a]) = Nothing  -- already flat, its own root is the head
appSpine :: Term -> Maybe (Term, [Term])
appSpine (f0 :@ x0) = Just (go [x0] f0)
  where
    go acc (g :@ y) = go (y : acc) g
    go acc g = (g, acc)
appSpine _ = Nothing

-- Number of arguments the left-hand side of a rule matches.
ruleArity :: Rule -> Int
ruleArity (F _ as, _) = length as
ruleArity (V _, _) = 0

-- Rename every variable apart by appending a suffix.
-- renameTerm "'" (F "f" [V "x", V "y"]) = F "f" [V "x'", V "y'"]
renameTerm :: Name -> Term -> Term
renameTerm suffix (V x) = V (x <> suffix)
renameTerm suffix (F f ts) = F f [renameTerm suffix t | t <- ts]

-- D(R) = {root(l) | l -> r ∈ R}
definedSymbols :: TRS -> [Name]
definedSymbols trs = nub [f | (F f _, _) <- trs]

-- Pos(t), in pre-order.
-- positions (F "add" []) = [[]]
-- positions (F "add" [V "x"]) = [[], [0]]
-- positions (F "add" [V "x", V "y"]) = [[], [0], [1]]
-- positions (F "add" [F "add" [V "x", V "y"], V "y", V "z"]) = [[], [0], [0,0], [0,1], [1], [2]]
positions :: Term -> [Position]
positions (V _) = [[]] -- {ε}
positions (F _ ts) = [] : [i : p | (i, t) <- zip [0 ..] ts, p <- positions t]

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
variables :: Term -> [Name]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]

-- substitute t sigma = t sigma
--
-- An application whose head turns into a concrete function symbol is flattened by mkApp:
--   substitute (V "f" :@ V "x") [("f", F "add1" [])] = F "add1" [V "x"]
substitute :: Term -> Subst -> Term
substitute (V x) sigma = fromMaybe (V x) (lookup x sigma)
substitute (f :@ x) sigma = mkApp (substitute f sigma) (substitute x sigma)
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- theta = compose sigma tau
--       = {x ↦ t tau | (x ↦ t) ∈ sigma} ∪ {y ↦ t | (y ↦ t) ∈ tau, y ∉ Dom(sigma)}
-- substitute t (compose sigma tau) = substitute (substitute t sigma) tau
compose :: Subst -> Subst -> Subst
compose sigma tau = [(x, substitute t tau) | (x, t) <- sigma] ++ [b | b@(x, _) <- tau, x `notElem` dom]
  where
    dom = map fst sigma

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
