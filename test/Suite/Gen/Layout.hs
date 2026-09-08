module Suite.Gen.Layout
  ( TokenSoup (..),
    LayoutSource (..),
    GDecl (..),
    Style (..),
    renderLayoutSource,
    renderStyled,
  )
where

import HS.Error (SrcPos (..))
import HS.Lexer
import Test.QuickCheck

-- A tiny declaration language, rendered either with explicit braces or with
-- layout, so that (P4) can be checked on generated input rather than only on
-- the three sources the spec names.
data GDecl
  = GEq String String
  | GWhere String String [GDecl]
  | GCase String String [(String, String)]
  deriving (Eq, Show)

data Style = Explicit | Layout
  deriving (Eq, Show)

newtype LayoutSource = LayoutSource [GDecl]

instance Show LayoutSource where
  show (LayoutSource ds) = renderLayoutSource Layout ds

renderLayoutSource :: Style -> [GDecl] -> String
renderLayoutSource = renderStyled

renderStyled :: Style -> [GDecl] -> String
renderStyled Explicit ds =
  "{ " ++ intercalate' " ; " (map one ds) ++ " }\n"
  where
    one (GEq n e) = n ++ " = " ++ e
    one (GWhere n e ws) =
      n ++ " = " ++ e ++ " where { " ++ intercalate' " ; " (map one ws) ++ " }"
    one (GCase n s alts) =
      n ++ " s = case s of { " ++ intercalate' " ; " (map alt alts) ++ " }"
      where
        alt (p, e) = p ++ " -> " ++ e
renderStyled Layout ds = concatMap one ds
  where
    one (GEq n e) = n ++ " = " ++ e ++ "\n"
    one (GWhere n e ws) =
      n ++ " = " ++ e ++ "\n  where\n" ++ concatMap (indent 4) ws
    one (GCase n _ alts) =
      n ++ " s = case s of\n" ++ concatMap altLine alts
      where
        altLine (p, e) = "  " ++ p ++ " -> " ++ e ++ "\n"
    indent k (GEq n e) = replicate k ' ' ++ n ++ " = " ++ e ++ "\n"
    indent k d = replicate k ' ' ++ takeWhile (/= '\n') (one d) ++ "\n"

intercalate' :: String -> [String] -> String
intercalate' _ [] = ""
intercalate' _ [x] = x
intercalate' sep (x : xs) = x ++ sep ++ intercalate' sep xs

instance Arbitrary LayoutSource where
  arbitrary = do
    n <- choose (1, 3)
    LayoutSource <$> vectorOf n genDecl
  shrink (LayoutSource ds) =
    [LayoutSource ds' | ds' <- shrinkList (const []) ds, not (null ds')]

genDecl :: Gen GDecl
genDecl =
  oneof
    [ GEq <$> name <*> expr,
      GWhere <$> name <*> expr <*> (vectorOf 1 (GEq <$> name <*> expr)),
      GCase <$> name <*> expr <*> alts
    ]
  where
    name = elements ["f", "g", "h", "k"]
    expr = elements ["x", "y", "Z", "S x"]
    alts = do
      k <- choose (1, 2)
      vectorOf k ((,) <$> elements ["Nil", "A", "B"] <*> expr)

-- Layout is a total token-to-token function, so the insertion-only invariant
-- must hold on nonsense too, and the termination check needs the pathological
-- column patterns a structured generator will not produce.
newtype TokenSoup = TokenSoup [PosToken]

instance Show TokenSoup where
  show (TokenSoup ts) = unwords [showToken (ptTok t) | t <- ts]

instance Arbitrary TokenSoup where
  arbitrary = TokenSoup . assignPositions <$> listOf genTok
  shrink (TokenSoup ts) =
    [TokenSoup (assignPositions ts') | ts' <- shrinkList (const []) (map ptTok ts)]

genTok :: Gen Token
genTok =
  elements
    ( map TKeyword ["where", "let", "in", "of", "do"]
        ++ map TSpecial "{};,"
        ++ map TReservedOp ["=", "->", "|"]
        ++ map TVarId ["x", "y", "f"]
        ++ [TConId "A", TInt 1]
    )

assignPositions :: [Token] -> [PosToken]
assignPositions = go 1 1
  where
    go _ _ [] = []
    go l c (t : ts) =
      PosToken (SrcPos l c) t : rest
      where
        rest = case ts of
          [] -> []
          _ -> nextOf ts
        nextOf us =
          let w = max 1 (length (showToken t))
           in if odd (l * 7 + c * 3 + w)
                then go (l + 1) (1 + (w * 3) `mod` 8) us
                else go l (c + w + 1) us
