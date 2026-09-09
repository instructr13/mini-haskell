{-# LANGUAGE OverloadedStrings #-}

module HS.Pretty (prettyTerm) where

import HS.Operator
import Prettyprinter
import TRS (appSpine)
import Term

data Side = SideLeft | SideNone | SideRight deriving (Eq, Show)

data Ctx = Ctx
  { ctxPrec :: Int,
    ctxSide :: Side,
    ctxOp :: String
  }

sugarPeano :: Term -> Maybe Int
sugarPeano (F "Succ" [f]) = fmap (+ 1) (sugarPeano f)
sugarPeano (F "Zero" []) = Just 0
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

listSpine :: Term -> Maybe [Term]
listSpine (F "Nil" []) = Just []
listSpine (F "Cons" [t, ts]) = (t :) <$> listSpine ts
listSpine _ = Nothing

sugarList :: Term -> Maybe (Doc ann)
sugarList t = do
  ts <- listSpine t

  pure (brackets (hsep (punctuate "," ([prettyTerm t' | t' <- ts]))))

sugarTuple :: Term -> Maybe (Doc ann)
sugarTuple (F "Unit" []) = Just "()"
sugarTuple (F "Tuple2" [x, y]) = Just (parens (hsep (punctuate "," ([prettyTerm t | t <- [x, y]]))))
sugarTuple _ = Nothing

prettyPrec :: Bool -> Ctx -> Term -> Doc ann
prettyPrec p c t
  -- Sugaring
  | Just n <- sugarPeano t = pretty n
  | Just d <- sugarList t = d
  | Just d <- sugarTuple t = d
  -- Operator conversion
  | F f [x, y] <- t, Just op <- lookup f operatorsByFunctor = prettyInfix p c op x y
  -- An application whose head is not a symbol yet is still juxtaposition
  | Just (h, args) <- appSpine t =
      parensIf p (hang 2 (sep (prettyPrec True c h : map (prettyPrec True c) args)))
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

prettyTerm :: Term -> Doc ann
prettyTerm = prettyPrec False c0
  where
    c0 = Ctx 0 SideNone ""
