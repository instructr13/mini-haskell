module ApplicativeTRS.Desugar.List (desugarListExpr) where

import ApplicativeTRS.Syntax

desugarListExpr :: [SExpr] -> SExpr
desugarListExpr (t : ts) = SEApp (SEApp (SEIdent "Cons") t) (desugarListExpr ts)
desugarListExpr [] = SEIdent "Nil"
