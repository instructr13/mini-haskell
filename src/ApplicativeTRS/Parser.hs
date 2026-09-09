module ApplicativeTRS.Parser (parseApplicativeTRS) where

import ApplicativeTRS.Lexer
import ApplicativeTRS.Syntax
import ApplicativeTRS.TokenStream
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Void
import TRS.Error
import Text.Megaparsec hiding (Token)
import Util

type Parser = Parsec Void TokenStream

parseApplicativeTRS :: FilePath -> String -> Either TRSError AppSectionSet
parseApplicativeTRS file src = do
  toks <- mapLeft syntaxError (lexApplicativeTRS file src)

  parseTokens file src toks

syntaxError :: (TraversableStream s, VisualStream s) => ParseErrorBundle s Void -> TRSError
syntaxError e = SyntaxError (bundleErrorPos e) (errorBundlePretty e)

parseTokens :: FilePath -> String -> [PosToken] -> Either TRSError AppSectionSet
parseTokens file src toks = mapLeft syntaxError sectionSet
  where
    sectionSet = parse (pSectionSet <* eof) file (tokenStream src toks)

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = token (f . unvirtual . ptTok) Set.empty

tok :: Token -> Parser ()
tok t = satisfyT check <?> showToken t
  where
    check :: Token -> Maybe ()
    check t' = if t' == unvirtual t then Just () else Nothing

ident :: Parser String
ident = satisfyT check <?> "identifier"
  where
    check :: Token -> Maybe String
    check (TIdent s') = Just s'
    check _ = Nothing

keyword :: String -> Parser ()
keyword s = tok (TKeyword s)

op :: String -> Parser ()
op s = tok (TOp s)

special :: Char -> Parser ()
special c = tok (TSpecial c)

semi :: Parser ()
semi = special ';'

itemsOf :: Parser a -> Parser [a]
itemsOf p = skipMany semi *> many (p <* skipMany semi)

parens :: Parser a -> Parser a
parens = between (special '(') (special ')')

pVarSec :: Parser [String]
pVarSec = parens $ do
  _ <- keyword "VAR"

  nub <$> many ident

pSimpleExpression :: Parser SExpr
pSimpleExpression = SEIdent <$> ident <|> parens pTerm

pTerm :: Parser SExpr
pTerm = foldl1 SEApp <$> some pSimpleExpression

pRule :: Parser AppRule
pRule = do
  lhs <- pTerm
  _ <- op "->"
  rhs <- pTerm
  _ <- semi

  pure (lhs, rhs)

pRulesSec :: Parser [AppRule]
pRulesSec = parens $ do
  _ <- keyword "RULES"

  itemsOf pRule

pSectionSet :: Parser AppSectionSet
pSectionSet = do
  vars <- fromMaybe [] <$> optional pVarSec
  rs <- pRulesSec

  pure (AppSectionSet {ssVars = vars, ssRules = rs})
