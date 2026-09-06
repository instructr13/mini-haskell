module TRS (module TRS) where

import Data.List (intercalate, nub)
import List

data Term = V String | F String [Term] deriving (Eq)

type Position = [Int]

type Subst = [(String, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")

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
positions (F f (t : ts)) = [] : [0 : p | p <- positions t] ++ [p + 1 : ps' | (p : ps') <- positions (F f ts)]

-- subTermAt t p = t|_p
-- subTermAt (F "add" [V "x", V "y"]) [] = F "add" [V "x", V "y"]
-- subTermAt (F "add" [V "x", V "y"]) [0] = V "x"
-- subTermAt (F "add" [V "x", V "y"]) [1] = V "y"
-- subTermAt (F "add" [(F "add" [V "x", V "y"]), V "y"]) [0, 0] = V "x"
subTermAt :: Term -> Position -> Term
subTermAt (V _) _ = error "subTermAt: cannot use this to a variable"
subTermAt t [] = t
subTermAt (F _ ts) (p : ps) = subTermAt (ts !! p) ps

-- replace t u p = t[u]_p
-- replace (F "add" [V "x", V "y"]) (F "0" []) [0] = F "add" [(F "0" []), V "y"]
-- replace (F "add" [(F "add" [V "x", V "y"]), V "y"]) (F "0" []) [0, 0]
--   = F "add" [F "add" [F "0" [], V "y"], V "y"]
replace :: Term -> Term -> Position -> Term
replace (V _) _ _ = error "replace: cannot use this to a variable"
replace _ u [] = u
replace (F f ts) u (p : ps) = F f (replaceAt p newT ts)
  where
    newT = replace (ts !! p) u ps

variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]

-- substitute t sigma = t sigma
substitute :: Term -> Subst -> Term
substitute (V x) sigma
  | Just t <- lookup x sigma = t
  | otherwise = V x
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- Pattern match without check
matchAsIs :: Term -> Term -> Maybe Subst
matchAsIs (V s) t = Just [(s, t)] -- Rule IV (w/o check)
matchAsIs (F f1 ts1) (F f2 ts2)
  -- Rule I (assume f1 and f2 are THE SAME)
  | Just matches <- maybeMatches, f1 == f2 && length ts1 == length ts2 = Just (concat matches)
  | otherwise = Nothing -- Rule II
  where
    maybeMatches = sequence [matchAsIs t1 t2 | (t1, t2) <- zip ts1 ts2]
matchAsIs (F _ _) (V _) = Nothing -- Rule III

-- Rule IV check (don't care about complexity)
allValidMatch :: Subst -> Bool
allValidMatch [] = True
allValidMatch (x : xs)
  | (xs1, xt1) <- x, Just xt2 <- lookup xs1 xs, xt1 /= xt2 = False
  | otherwise = allValidMatch xs

-- match s t = Just sigma, if s sigma = t for some sigma
-- match s t = Nothing, otherwise
-- match (F "add" [V "x", (F "s" [(F "add" [V "y", V "z"])])]) (F "add" [(F "s" [V "y"]), (F "s" [(F "add" [(F "add" [V "x", (F "0" [])]), V "z"])])])
--   = Just [("x", F "s" [V "y"]), ("y", F "add" [V "x", (F "0" [])]), ("z", V "z")]
-- match (F "add" [(F "s" [V "x"]), (F "add" [V "x", V "y"])]) (F "add" [(F "s" [(F "add" [(F "0" []), V "x"])]), (F "add" [(F "add" [(F "0" []), (F "0" [])]), V "x"])])
--   = Nothing
match :: Term -> Term -> Maybe Subst
match s t
  | Just matches' <- matches, allValidMatch matches' = matches
  | otherwise = Nothing
  where
    matches = matchAsIs s t

findTRSMatch :: TRS -> Term -> Maybe (Rule, Subst)
findTRSMatch [] _ = Nothing
findTRSMatch (rule@(l, _) : trs) t
  | Just subst <- maybeSubst = Just (rule, subst)
  | otherwise = findTRSMatch trs t
  where
    maybeSubst = match l t

findFirstTRSMatch :: TRS -> [(Position, Term)] -> Maybe (Position, Rule, Subst)
findFirstTRSMatch [] _ = Nothing
findFirstTRSMatch _ [] = Nothing
findFirstTRSMatch trs ((p, t) : ts)
  | Just (rule, subst) <- maybeRule = Just (p, rule, subst)
  | otherwise = findFirstTRSMatch trs ts
  where
    maybeRule = findTRSMatch trs t

-- rewrite R t = Just u, if t ->_R u for some term u
-- rewrite R t = Nothing, otherwise
-- 1. Get positions for t
-- 2. Get all subterms for t --> ss
-- 3. Pattern match for all first TRS with s (of ss) and l
-- 4. If found, find sigma for s and l (return Nothing if not found)
-- 5. replace t[l sigma]_p -> t[r sigma]_p and return with Just
rewrite :: TRS -> Term -> Maybe Term
rewrite trs t
  | Just (p, (_, r), subst) <- maybeMatch = Just (replace t (substitute r subst) p)
  | otherwise = Nothing
  where
    subTerms = [(p, subTermAt t p) | p <- positions t]
    maybeMatch = findFirstTRSMatch trs subTerms

-- nf R t = u if t ->_R ... ->_R u for some normal form u
nf :: TRS -> Term -> Term
nf trs t
  | Just u' <- u, t == u' = t
  | Just u' <- u, otherwise = nf trs u'
  | Nothing <- u = t
  where
    u = rewrite trs t

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
