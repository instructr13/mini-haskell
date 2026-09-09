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

fromPeano :: Term -> Maybe Int
fromPeano (F "s" [f]) = fmap (+ 1) (fromPeano f)
fromPeano (F "0" []) = Just 0
fromPeano _ = Nothing

prettyInfix :: Bool -> Ctx -> OpSpec -> Term -> Term -> Doc ann
prettyInfix p c (OpSpec {opFunctor = fn, opSymbol = sym, opPrec = prec, opAssoc = assoc}) l r =
  if needsParens
    then parens body
    else body
  where
    lc = Ctx {ctxPrec = prec, ctxSide = SideLeft, ctxOp = fn}
    rc = Ctx {ctxPrec = prec, ctxSide = SideRight, ctxOp = fn}

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

prettyPrec :: Bool -> Ctx -> Term -> Doc ann
prettyPrec p c t
  | Just n <- fromPeano t = pretty n
  | F f [x, y] <- t, Just op <- lookup f operatorsByFunctor = prettyInfix p c op x y
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
    c0 = Ctx {ctxPrec = 0, ctxSide = SideNone, ctxOp = ""}
