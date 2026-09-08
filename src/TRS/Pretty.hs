{-# LANGUAGE OverloadedStrings #-}

-- | 項の表示。
--
--   Peano 数と Cons 鎖はそのまま出すと読めないので、'Notation' で与えられた
--   構成子名に一致する形を数値リテラルと @[a, b]@ に畳んで表示する。
--   畳むのは表示だけで、項そのものは変えない。
module TRS.Pretty
  ( Notation (..),
    plainNotation,
    prettyTerm,
    prettyTermWith,
    prettyRuleWith,
    prettyTRS,
    prettyTRSNotation,
    prettyPosition,
    prettyRuleRef,
    prettyRedex,
    numeralOf,
    consChain,
  )
where

import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Prettyprinter
import Render
import TRS

-- Which symbols spell out the numerals and lists that get folded back into
-- literal notation. Empty lists switch the folding off.
data Notation = Notation
  { noZero :: [String],
    noSucc :: [String],
    noNil :: [String],
    noCons :: [String]
  }
  deriving (Eq, Show)

plainNotation :: Notation
plainNotation = Notation [] [] [] []

-- | @Succ (Succ Zero)@ as the number it denotes.
numeralOf :: Notation -> Term -> Maybe Integer
numeralOf n = go
  where
    go (F f []) | f `elem` noZero n = Just 0
    go (F f [t]) | f `elem` noSucc n = (+ 1) <$> go t
    go _ = Nothing

-- | A @Cons@ chain as its elements plus, when the chain does not end in
--   @Nil@, whatever it ends in. @[a, b]@ for a proper list, @a : b : t@ for
--   an improper one -- which is what most rule left-hand sides are.
consChain :: Notation -> Term -> Maybe ([Term], Maybe Term)
consChain n t = case go t of
  ([], _) -> Nothing
  (xs, tl) -> Just (xs, tl)
  where
    go (F f [x, xs]) | f `elem` noCons n = let (ys, tl) = go xs in (x : ys, tl)
    go (F f []) | f `elem` noNil n = ([], Nothing)
    go u = ([], Just u)

isNil :: Notation -> Term -> Bool
isNil n (F f []) = f `elem` noNil n
isNil _ _ = False

prettyTerm :: Term -> Doc Ann
prettyTerm = prettyTermWith plainNotation Set.empty

prettyTermWith :: Notation -> Set String -> Term -> Doc Ann
prettyTermWith notation defined = go
  where
    go t
      | isNil notation t = bracketed mempty
      | Just k <- numeralOf notation t = literal (pretty k)
      | Just (xs, Nothing) <- consChain notation t =
          bracketed (commaSepWrap (map go xs))
      | Just (xs, Just tl) <- consChain notation t =
          group (align (sep (punctuate (space <> operator ":") (map atom (xs ++ [tl])))))
    go (V x) = varName (pretty x)
    go (F f []) = symbol f
    go (F f ts) = group (symbol f <> parenthesized (nest 2 (commaSepWrap (map go ts))))
    -- An operand of ':' needs parentheses only if it is itself a chain.
    atom u = case consChain notation u of
      Just (_, Just _) -> parenthesized (go u)
      _ -> go u
    symbol f
      | Set.member f defined = funName (pretty f)
      | otherwise = conName (pretty f)

prettyRuleWith :: Notation -> Set String -> Rule -> Doc Ann
prettyRuleWith notation defined (l, r) =
  group (term l <+> operator "->" <> nest 2 (line <> term r))
  where
    term = prettyTermWith notation defined

prettyTRS :: TRS -> Doc Ann
prettyTRS = prettyTRSNotation plainNotation

prettyTRSNotation :: Notation -> TRS -> Doc Ann
prettyTRSNotation notation trs =
  vsep (map (prettyRuleWith notation (definedSymbols trs)) trs)

prettyPosition :: Position -> Doc Ann
prettyPosition [] = "\949"
prettyPosition p = pretty (intercalate "." (map show p))

prettyRuleRef :: RuleRef -> Doc Ann
prettyRuleRef (ByRule i _) = punct "#" <> number i
prettyRuleRef (Builtin name) = faint "builtin" <+> funName (pretty name)

prettyRedex :: Redex -> Doc Ann
prettyRedex (Redex p ref) = "at" <+> prettyPosition p <+> "by" <+> prettyRuleRef ref
