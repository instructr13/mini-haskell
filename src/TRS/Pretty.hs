{-# LANGUAGE OverloadedStrings #-}

module TRS.Pretty (prettyRule, prettyTRS) where

import HS.Pretty (prettyTerm)
import Prettyprinter
import TRS

prettyRule :: Rule -> Doc ann
prettyRule (l, r) =
  group (prettyTerm l <+> "=" <> nest 2 (line <> prettyTerm r))

prettyTRS :: TRS -> Doc ann
prettyTRS trs = vsep (map prettyRule trs)
