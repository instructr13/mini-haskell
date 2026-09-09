module ApplicativeTRS.Desugar (pDesugarExpression) where

import ApplicativeTRS.Desugar.List (desugarListExpr)
import ApplicativeTRS.Desugar.Peano (desugarPeanoSExpr)
import ApplicativeTRS.Desugar.Tuple (desugarTuple)
import ApplicativeTRS.Lexer
import ApplicativeTRS.Syntax
import Text.Megaparsec hiding (Token)

pUIntLiteral :: Parser Int
pUIntLiteral = satisfyT check <?> "numbers"
  where
    check :: Token -> Maybe Int
    check (TNumLiteral n) = Just n
    check _ = Nothing

pDesugarPeano :: Parser SExpr
pDesugarPeano = desugarPeanoSExpr <$> pUIntLiteral

pListOfSExprs :: Parser SExpr -> Parser [SExpr]
pListOfSExprs pTerm = do
  es <- squareBrackets $ sepBy pTerm (special ',')

  pure es

pDesugarListExpr :: Parser SExpr -> Parser SExpr
pDesugarListExpr pTerm = desugarListExpr <$> pListOfSExprs pTerm

pDesugarTuple :: Parser SExpr -> Parser SExpr
pDesugarTuple pTerm = do
  es <- parens $ sepBy pTerm (special ',')

  pure (desugarTuple es)

pDesugarExpression :: Parser SExpr -> Parser SExpr
pDesugarExpression pTerm = pDesugarPeano <|> pDesugarListExpr pTerm <|> pDesugarTuple pTerm
