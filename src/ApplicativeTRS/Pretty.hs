{-# LANGUAGE OverloadedStrings #-}

module ApplicativeTRS.Pretty (prettyApplicativeTerm) where

import Prettyprinter
import Term

data Assoc = AssocLeft | AssocNone | AssocRight deriving (Eq, Show)

data Side = SideLeft | SideNone | SideRight deriving (Eq, Show)

data Ctx = Ctx
  { ctxPrec :: Int,
    ctxSide :: Side,
    ctxOp :: String
  }

data OpSpec = OpSpec
  { opFunctor :: String,
    opSymbol :: String,
    opPrec :: Int,
    opAssoc :: Assoc
  }

operators :: [OpSpec]
operators =
  [ OpSpec "add" "+" 6 AssocLeft,
    OpSpec "cons" ":" 5 AssocRight,
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

operatorsByFunctor :: [(String, OpSpec)]
operatorsByFunctor = [(opFunctor o, o) | o <- operators]

sugarPeano :: Term -> Maybe Int
sugarPeano (F "s" [f]) = fmap (+ 1) (sugarPeano f)
sugarPeano (F "0" []) = Just 0
sugarPeano _ = Nothing

prettyInfix :: Bool -> Ctx -> OpSpec -> Term -> Term -> Doc ann
prettyInfix p c (OpSpec {opFunctor = fn, opSymbol = sym, opPrec = prec, opAssoc = assoc}) l r =
  if needsParens
    then parens body
    else body
  where
    lc = Ctx prec SideLeft fn
    rc = Ctx prec SideRight fn

    needsParens =
      p || case ctxSide c of
        SideNone -> False
        side -> case compare (ctxPrec c) prec of
          GT -> True
          LT -> False
          EQ
            | ctxOp c /= fn -> True
            | otherwise -> case (assoc, side) of
                (AssocLeft, SideLeft) -> False
                (AssocRight, SideRight) -> False
                _ -> True

    body = prettyPrec False lc l <+> pretty sym <+> prettyPrec False rc r

sugarList :: Term -> Maybe (Doc ann)
sugarList (F "cons" [t, (F "cons" ts)]) = go ("[" <> prettyApplicativeTerm t) ts
  where
    go :: Doc ann -> [Term] -> Maybe (Doc ann)
    go acc [t', F "cons" ts'] = go (acc <> "," <+> prettyApplicativeTerm t') ts'
    go acc [t', F "nil" []] = Just (acc <> "," <+> prettyApplicativeTerm t' <> "]")
    go _ _ = Nothing
sugarList _ = Nothing

sugarNil :: Term -> Maybe (Doc ann)
sugarNil (F "nil" []) = Just "[]"
sugarNil _ = Nothing

prettyPrec :: Bool -> Ctx -> Term -> Doc ann
prettyPrec p c t
  -- Sugaring
  | Just n <- sugarPeano t = pretty n
  | Just d <- sugarList t = d
  | Just d <- sugarNil t = d
  -- Operator conversion
  | F f [x, y] <- t, Just op <- lookup f operatorsByFunctor = prettyInfix p c op x y
  -- Juxtaposition processing
  | (h, []) <- flatten t = pretty h
  | (h, args) <- flatten t =
      parensIf p (hang 2 (sep (pretty h : map (prettyPrec True c) args)))
  where
    parensIf :: Bool -> Doc ann -> Doc ann
    parensIf b = if b then parens else id

    flatten :: Term -> (String, [Term])
    flatten (V x) = (x, [])
    flatten (F f ts) = (f, ts)

prettyApplicativeTerm :: Term -> Doc ann
prettyApplicativeTerm = prettyPrec False c0
  where
    c0 = Ctx 0 SideNone ""
