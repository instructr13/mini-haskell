module Term (Term (..)) where

import Data.List (intercalate)
import Prettyprinter

data Term = V String | F String [Term] deriving (Eq)

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")

instance Pretty Term where
  pretty = go False
    where
      go :: Bool -> Term -> Doc ann
      go _ t | (h, []) <- flatten t = pretty h
      go p t
        | (h, args) <- flatten t = case fromPeano (F h args) of
            Just n -> pretty n
            _ -> parensIf p (hang 2 (sep (pretty h : map (go True) args)))

      parensIf :: Bool -> Doc ann -> Doc ann
      parensIf b = if b then parens else id

      flatten :: Term -> (String, [Term])
      flatten = go' []
        where
          go' acc (V x) = (x, acc)
          go' acc (F f bs) = (f, bs ++ acc)

fromPeano :: Term -> Maybe Int
fromPeano (F "s" [f]) = fmap (+ 1) (fromPeano f)
fromPeano (F "0" []) = Just 0
fromPeano _ = Nothing
