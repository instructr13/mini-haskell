module ApplicativeTRS.Elab (elab, elabRule) where

import ApplicativeTRS.Syntax
import TRS

elab :: [String] -> SExpr -> Term
elab vars = go []
  where
    go acc (SEApp f x) = go (go [] x : acc) f
    go acc (SEIdent x)
      | x `elem` vars = apps (V x) acc
      | otherwise = F x acc

elabRule :: [String] -> AppRule -> Rule
elabRule vars (l, r) = ((elab vars l), (elab vars r))
