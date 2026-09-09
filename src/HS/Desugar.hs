module HS.Desugar (pDesugared) where

import HS.Desugar.List
import HS.Desugar.Peano
import HS.Desugar.Tuple (desugarTuple)
import HS.Lexer
import HS.Syntax
import Text.Megaparsec hiding (Token)

pUIntLiteral :: Parser Int
pUIntLiteral = satisfyT check <?> "numbers"
  where
    check :: Token -> Maybe Int
    check (TNumLiteral n) = Just n
    check _ = Nothing

pDesugared :: (ConLike a) => Parser a -> Parser a
pDesugared pElem =
  desugarPeano
    <$> pUIntLiteral
      <|> desugarListExpr
    <$> squareBrackets (sepBy pElem (special ','))
      <|> desugarTuple
    <$> parens (sepBy pElem (special ','))
