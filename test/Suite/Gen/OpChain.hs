module Suite.Gen.OpChain
  ( Atom (..),
    OpChain (..),
    flattenChain,
    noChain,
    testOps,
    testFixities,
    showExpr,
  )
where

import qualified Data.Map.Strict as Map
import HS.Fixity (Fixity (..), FixityEnv)
import HS.Name
import HS.Pretty (Prec (..), prettyExpr)
import Render (renderOneLine)
import HS.Syntax
import Test.QuickCheck

data Atom = AOperand String | AOp String
  deriving (Eq, Show)

showExpr :: Expr -> String
showExpr = renderOneLine . prettyExpr PTop

testOps :: [(String, Fixity)]
testOps =
  [ ("+", Fixity AssocLeft 6),
    ("-", Fixity AssocLeft 6),
    ("*", Fixity AssocLeft 7),
    (":", Fixity AssocRight 5),
    ("++", Fixity AssocRight 5),
    ("&&", Fixity AssocRight 3)
  ]

testFixities :: FixityEnv
testFixities = Map.fromList testOps

opNames :: [String]
opNames = map fst testOps

flattenChain :: Expr -> [Atom]
flattenChain e = case e of
  EOpChain e0 rs ->
    flattenChain e0 ++ concat [AOp op : flattenChain x | (op, x) <- rs]
  EApply (EApply (EVar (VarName op)) l) r
    | op `elem` opNames -> flattenChain l ++ [AOp op] ++ flattenChain r
  EApply (EApply (ECon (ConName op)) l) r
    | op `elem` opNames -> flattenChain l ++ [AOp op] ++ flattenChain r
  _ -> [AOperand (showExpr e)]

noChain :: Expr -> Bool
noChain e = case e of
  EOpChain _ _ -> False
  EApply f x -> noChain f && noChain x
  ESectionL _ x -> noChain x
  ESectionR _ x -> noChain x
  EIf a b c -> noChain a && noChain b && noChain c
  ECase s alts -> noChain s && all (rhsOk . altBody) alts
  ELet _ b -> noChain b
  ELambda _ b -> noChain b
  EList xs -> all noChain xs
  ETuple xs -> all noChain xs
  EVar _ -> True
  ECon _ -> True
  ELiteral _ -> True
  where
    rhsOk (Plain x) = noChain x
    rhsOk (Guarded gs) = all (\(g, x) -> noChain g && noChain x) gs

newtype OpChain = OpChain Expr

instance Show OpChain where show (OpChain e) = showExpr e

instance Arbitrary OpChain where
  arbitrary = sized $ \n -> do
    k <- choose (1, 1 + min n 6)
    e0 <- genOperand 0
    rs <- vectorOf k ((,) <$> elements opNames <*> genOperand 0)
    pure (OpChain (EOpChain e0 rs))
  shrink (OpChain (EOpChain e0 rs)) =
    [OpChain (EOpChain e0 rs') | rs' <- shrinkList (const []) rs, not (null rs')]
  shrink _ = []

genOperand :: Int -> Gen Expr
genOperand d =
  frequency
    [ (6, atomic),
      (2, EApply (EVar (VarName "f")) <$> atomic),
      (if d >= 1 then 0 else 2, nested)
    ]
  where
    atomic =
      oneof
        [ ELiteral . LInt <$> choose (0, 9),
          EVar . VarName <$> elements ["a", "b", "c", "d", "e"],
          pure (ECon (ConName "Nil"))
        ]
    nested = do
      k <- choose (1, 2)
      e0 <- genOperand (d + 1)
      rs <- vectorOf k ((,) <$> elements opNames <*> genOperand (d + 1))
      pure (EOpChain e0 rs)
