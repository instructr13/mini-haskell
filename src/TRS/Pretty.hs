{-# LANGUAGE OverloadedStrings #-}

module TRS.Pretty (prettyRule, prettyTRS) where

import Prettyprinter
import TRS

prettyRule :: Rule -> Doc ann
prettyRule (l, r) =
  group (pretty l <+> "->" <> nest 2 (line <> pretty r))

prettyTRS :: TRS -> Doc ann
prettyTRS trs = vsep (map prettyRule trs)
