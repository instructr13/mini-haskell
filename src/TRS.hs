module TRS where

import Data.List

data Term = V String | F String [Term] deriving (Eq)

type Subst = [(String, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

type Position = [Int]

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
