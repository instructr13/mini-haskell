{-# LANGUAGE OverloadedStrings #-}

module TRS.Pretty
  ( prettyTerm,
    prettyTermWith,
    prettyRule,
    prettyRuleWith,
    prettyTRS,
    prettyTRSWith,
    showTermPlain,
  )
where

import Prettyprinter
import Render
import TRS

prettyTerm :: Term -> Doc Ann
prettyTerm = prettyTermWith []

prettyTermWith :: [String] -> Term -> Doc Ann
prettyTermWith defined = go
  where
    go (V x) = varName (pretty x)
    go (F f []) = symbol f
    go (F f ts) =
      group (symbol f <> punct "(" <> nest 2 (args ts) <> punct ")")
    args ts =
      align (sep (punctuate (punct ",") (map go ts)))
    symbol f
      | f `elem` defined = funName (pretty f)
      | otherwise = conName (pretty f)

prettyRule :: Rule -> Doc Ann
prettyRule = prettyRuleWith []

prettyRuleWith :: [String] -> Rule -> Doc Ann
prettyRuleWith defined (l, r) =
  group (prettyTermWith defined l <+> operator "->" <> nest 2 (line <> prettyTermWith defined r))

prettyTRS :: TRS -> Doc Ann
prettyTRS trs = prettyTRSWith (definedSymbols trs) trs

prettyTRSWith :: [String] -> TRS -> Doc Ann
prettyTRSWith defined trs = vsep (map (prettyRuleWith defined) trs)

showTermPlain :: Term -> String
showTermPlain = show
