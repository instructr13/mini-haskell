module ApplicativeTRS.Wild (expandAppModule) where

import ApplicativeTRS.Syntax
import TRS.Error
import Wild

number :: Int -> SExpr -> SExpr
number n e = snd (go n e)
  where
    go :: Int -> SExpr -> (Int, SExpr)
    go i SEWild = (i + 1, SEIdent (wildVar i))
    go i (SEApp f x) = (i'', SEApp f' x')
      where
        (i', f') = go i f
        (i'', x') = go i' x
    go i e' = (i, e')

hasWild :: SExpr -> Bool
hasWild SEWild = True
hasWild (SEApp l r) = hasWild l || hasWild r
hasWild _ = False

-- Is the rule starts with _ ?
wildHead :: SExpr -> Bool
wildHead (SEApp SEWild _) = True
wildHead (SEApp f _) = wildHead f
wildHead SEWild = True
wildHead _ = False

expandRule :: AppRule -> Either TRSError AppRule
expandRule (l, r)
  | hasWild r = Left (Invalid "_ cannot appear on the rhs")
  | wildHead l = Left (Invalid "lhs of a rule cannot be _")
  | otherwise = Right (number 0 l, r)

expandAppModule :: AppModule -> Either TRSError AppModule
expandAppModule m = do
  newRules <- sequence [expandRule r | r <- amRules m]

  pure (AppModule (amData m) newRules)
