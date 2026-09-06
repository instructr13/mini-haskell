module TRS (module TRS) where

import Data.List (intercalate)
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

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
