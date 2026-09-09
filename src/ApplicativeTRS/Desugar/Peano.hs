module ApplicativeTRS.Desugar.Peano (desugarPeanoSExpr) where

import ApplicativeTRS.Syntax

desugarPeanoSExpr :: Int -> SExpr
desugarPeanoSExpr 0 = SEIdent "0"
desugarPeanoSExpr i
  | i < 0 = error "desugarPeanoSExpr: value must be a positive integer"
  | otherwise = SEApp (SEIdent "s") (desugarPeanoSExpr (i - 1))
