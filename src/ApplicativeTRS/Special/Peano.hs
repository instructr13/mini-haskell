module ApplicativeTRS.Special.Peano (toPeanoSExpr) where

import ApplicativeTRS.Syntax

toPeanoSExpr :: Int -> SExpr
toPeanoSExpr 0 = SEIdent "0"
toPeanoSExpr i
  | i < 0 = error "toPeano: value must be a positive integer"
  | otherwise = SEApp (SEIdent "s") (toPeanoSExpr (i - 1))
