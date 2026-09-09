module ApplicativeTRS.Desugar.Tuple (desugarTuple) where

import ApplicativeTRS.Syntax

desugarTuple :: [SExpr] -> SExpr
desugarTuple [x, y] = SEApp (SEApp (SEIdent "Tuple2") x) y
desugarTuple [x] = x -- Pass parens with only 1 term which is not a tuple
desugarTuple [] = SEIdent "Unit"
desugarTuple e = error ("desugarUnit: invalid tuple: " ++ show e)
