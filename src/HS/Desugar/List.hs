module HS.Desugar.List (desugarListExpr) where

import HS.Syntax

desugarListExpr :: (ConLike a) => [a] -> a
desugarListExpr (t : ts) = conApp "Cons" [t, desugarListExpr ts]
desugarListExpr [] = conApp "Nil" []
