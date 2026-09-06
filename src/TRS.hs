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
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

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
subTermAt t [] = t
subTermAt v@(V _) _ = v
subTermAt (F _ ts) (p : ps) = subTermAt (ts !! p) ps

-- replace t u p = t[u]_p
-- replace (F "add" [V "x", V "y"]) (F "0" []) [0] = F "add" [(F "0" []), V "y"]
-- replace (F "add" [(F "add" [V "x", V "y"]), V "y"]) (F "0" []) [0, 0]
--   = F "add" [F "add" [F "0" [], V "y"], V "y"]
replace :: Term -> Term -> Position -> Term
replace _ u [] = u
replace v@(V _) _ _ = v
replace (F f ts) u (p : ps) = F f (replaceAt p newT ts)
  where
    newT = replace (ts !! p) u ps

variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]

-- substitute t sigma = sigma
substitute :: Term -> Subst -> Term
substitute (V x) sigma
  | Just t <- lookup x sigma = t
  | otherwise = V x
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- Pattern match without check
matchAsIs :: Term -> Term -> Maybe Subst
matchAsIs (V s) t = Just [(s, t)] -- Rule IV (w/o check)
matchAsIs (F f1 ts1) (F f2 ts2)
  | f1 == f2 = Just matches -- Rule I (assume f1 and f2 are THE SAME)
  | otherwise = Nothing -- Rule II
  where
    matches = concat [m | Just m <- [matchAsIs t1 t2 | (t1, t2) <- zip ts1 ts2]]
matchAsIs (F _ _) (V _) = Nothing -- Rule III

-- Rule IV check (don't care about complexity)
allValidMatch :: Subst -> Bool
allValidMatch [] = True
allValidMatch (x : xs)
  | (xs1, xt1) <- x, Just xt2 <- lookup xs1 xs, xt1 /= xt2 = False
  | otherwise = allValidMatch xs

-- match s t = Just sigma, if s sigma = t for some sigma
-- match s t = Nothing, otherwise
-- match (F "add" [V "x", (F "s" [V "y", V "z"])]) (F "add" [(F "s" [V "y"]), (F "s" [(F "add" [(F "add" [V "x", (F "0" [])]), V "z"])])])
--   = Just [("x", F "s" [V "y"]), ("y", F "add" [V "x", (F "0" [])]), ("z", V "z")]
-- match (F "add" [(F "s" [V "x"]), (F "add" [V "x", V "y"])]) (F "add" [(F "s" [(F "add" [(F "0" []), V "x"])]), (F "add" [(F "add" [(F "0" []), (F "0" [])]), V "x"])])
--   = Nothing
match :: Term -> Term -> Maybe Subst
match s t
  | Just matches' <- matches, allValidMatch matches' = matches
  | otherwise = Nothing
  where
    matches = matchAsIs s t

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
