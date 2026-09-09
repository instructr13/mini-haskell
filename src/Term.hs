module Term (Term (..)) where

import Data.List (intercalate)

data Term = V String | F String [Term] deriving (Eq)

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")
