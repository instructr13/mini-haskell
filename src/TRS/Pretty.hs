{-# LANGUAGE OverloadedStrings #-}

module TRS.Pretty (prettyRule, prettyTRS) where

import ApplicativeTRS.Pretty (prettyApplicativeTerm)
import Prettyprinter
import TRS

prettyRule :: Rule -> Doc ann
prettyRule (l, r) =
  group (prettyApplicativeTerm l <+> "->" <> nest 2 (line <> prettyApplicativeTerm r))

prettyTRS :: TRS -> Doc ann
prettyTRS trs = vsep (map prettyRule trs)
