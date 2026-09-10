module HS.Wild (expandModule) where

import Data.List (mapAccumL)
import HS.Syntax
import Wild

number :: Int -> Pat -> (Int, Pat)
number n PWild = (n + 1, PVar (wildVar n))
number n (PCon c ps) = fmap (PCon c) (mapAccumL number n ps)
number n p' = (n, p')

expandRuleDecl :: RuleDecl -> RuleDecl
expandRuleDecl rd = rd {rdPats = snd (mapAccumL number 0 (rdPats rd))}

expandModule :: Module -> Module
expandModule m = Module (mData m) [expandRuleDecl rd | rd <- mRules m]
