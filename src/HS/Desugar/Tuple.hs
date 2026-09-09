module HS.Desugar.Tuple (desugarTuple) where

import HS.Syntax

desugarTuple :: (ConLike a) => [a] -> a
desugarTuple [x, y] = conApp "Tuple2" [x, y]
desugarTuple [x] = x -- Pass parens with only 1 term which is not a tuple
desugarTuple [] = conApp "Unit" []
desugarTuple _ = error "desugarUnit: invalid tuple"
