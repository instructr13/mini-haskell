module HS.Operator (Assoc (..), OpSpec (..), operators, operatorsByPrec, operatorsByFunctor) where

import Data.Function (on)
import Data.List (groupBy, sortOn)
import Data.Ord (Down (..))

data Assoc = AssocLeft | AssocNone | AssocRight deriving (Eq, Show)

-- A binary operator that desugars into an application of opFunctor.
data OpSpec = OpSpec
  { opFunctor :: String,
    opSymbol :: String,
    opPrec :: Int,
    opAssoc :: Assoc
  }

operators :: [OpSpec]
operators =
  [ OpSpec "compose" "." 9 AssocRight,
    OpSpec "mul" "*" 7 AssocLeft,
    OpSpec "add" "+" 6 AssocLeft,
    OpSpec "sub" "-" 6 AssocLeft,
    OpSpec "Cons" ":" 5 AssocRight, -- : is only the constructor "Cons"
    OpSpec "append" "++" 5 AssocRight,
    OpSpec "eq" "==" 4 AssocNone,
    OpSpec "neq" "/=" 4 AssocNone,
    OpSpec "leq" "<=" 4 AssocNone,
    OpSpec "lt" "<" 4 AssocNone,
    OpSpec "geq" ">=" 4 AssocNone,
    OpSpec "gt" ">" 4 AssocNone,
    OpSpec "and" "&&" 3 AssocRight,
    OpSpec "or" "||" 2 AssocRight
  ]

-- Grouped by precedence, tightest binding first, as makeExprParser expects.
operatorsByPrec :: [[OpSpec]]
operatorsByPrec = groupBy ((==) `on` opPrec) (sortOn (Down . opPrec) operators)

operatorsByFunctor :: [(String, OpSpec)]
operatorsByFunctor = [(opFunctor o, o) | o <- operators]
