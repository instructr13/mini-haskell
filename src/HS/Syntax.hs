module HS.Syntax (module HS.Syntax) where

import HS.Name

type Module = [Decl]

data Decl
  = DData TyConName [VarName] [(ConName, Int)]
  | DFun VarName [Clause]
  | DFixity Assoc Int [String]

-- Function definition
-- f p_1, ..., p_n = e
-- clWhere ==> ELet
data Clause = Clause
  { clPats :: [Pat],
    clBody :: Rhs,
    clWhere :: [Decl]
  }

data Rhs
  = Plain Expr -- f p = e
  | Guarded [(Expr, Expr)] -- f p | g_1 = e_1 | g_2 = e_2 ==> Plain + ECase

-- infixl, infixr, infix
data Assoc = AssocLeft | AssocRight | AssocNone
  deriving (Eq, Show)

-- [C] shows the expression is remained within core AST
data Pat
  = PVar VarName -- [C] x
  | PCon ConName [Pat] -- [C] (Cons x xs -> x : xs)
  | PWild -- _ ==> new PVar
  | PAs VarName Pat -- x@p ==> PVar & ELet
  | PLiteral Literal -- 0, 'a' ==> PCon & equality comparison
  | PList [Pat] -- [x, y] ==> ditto
  | PTuple [Pat] -- (x, y) ==> ditto

data Expr
  = EVar VarName -- [C] x, f, (++)
  | ECon ConName -- [C] Nil -> [], Cons -> :
  | EApply Expr Expr -- [C] f e
  | EOpChain Expr [(String, Expr)] -- a + b * c ==> tree of EApply
  | ESectionL String Expr -- (x +) ==> EApply or ELambda
  | ESectionR String Expr -- (+ x) ==> ditto
  | EIf Expr Expr Expr -- if c then a else b
  | ECase Expr [Alt] -- case e of ... ==> ELet + EApply ==>
  | ELet [Decl] Expr -- let ... in e
  | ELambda [Pat] Expr -- \x -> e
  | ELiteral Literal -- 0, 'a', "ab" ==> EApply to ECon
  | EList [Expr] -- [a, b, c] ==> ditto
  | ETuple [Expr] -- (a, b) ==> ditto

-- altWhere ==> ELet
data Alt = Alt {altPat :: Pat, altBody :: Rhs, altWhere :: [Decl]}

data Literal = LInt Integer | LChar Char | LString String
  deriving (Eq, Show)

clauseArity :: Clause -> Int
clauseArity = length . clPats

appSpine :: Expr -> (Expr, [Expr])
appSpine = go []
  where
    go acc (EApply f a) = go (a : acc) f
    go acc e = (e, acc)
