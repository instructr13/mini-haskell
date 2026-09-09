module ApplicativeTRS.Desugar.Peano (desugarPeanoSExpr) where

import ApplicativeTRS.Syntax

desugarPeanoSExpr :: Int -> SExpr
desugarPeanoSExpr 0 = SEIdent "Zero"
desugarPeanoSExpr i
  | i < 0 = error "desugarPeanoSExpr: value must be a positive integer"
  | otherwise = SEApp (SEIdent "Succ") (desugarPeanoSExpr (i - 1))
