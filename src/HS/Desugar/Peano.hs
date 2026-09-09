module HS.Desugar.Peano (desugarPeano) where

import HS.Syntax

desugarPeano :: (ConLike a) => Int -> a
desugarPeano i
  | i < 0 = error "desugarPeanoSExpr: value must be a positive integer"
  | i == 0 = conApp "Zero" []
  | otherwise = conApp "Succ" [desugarPeano (i - 1)]
