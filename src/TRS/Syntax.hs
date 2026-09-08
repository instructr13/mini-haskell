module TRS.Syntax (module TRS.Syntax) where

import TRS

data SectionSet = SectionSet {ssVars :: [String], ssRules :: [Rule]}
