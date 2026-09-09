module ApplicativeTRS.Desugar.List (desugarListExpr) where

import ApplicativeTRS.Syntax

desugarListExpr :: [SExpr] -> SExpr
desugarListExpr (t : ts) = SEApp (SEApp (SEIdent "cons") t) (desugarListExpr ts)
desugarListExpr [] = SEIdent "nil"
