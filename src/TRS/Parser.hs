module TRS.Parser (parseTRS) where

import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Void
import TRS
import TRS.Error
import TRS.Lexer
import TRS.Syntax
import TRS.TokenStream
import Text.Megaparsec hiding (Token)
import Util

type Parser = Parsec Void TokenStream

parseTRS :: FilePath -> String -> Either TRSError SectionSet
parseTRS file src = do
  toks <- mapLeft syntaxError (lexTRS file src)

  parseTokens file src toks

syntaxError :: (TraversableStream s, VisualStream s) => ParseErrorBundle s Void -> TRSError
syntaxError e = SyntaxError (bundleErrorPos e) (errorBundlePretty e)

parseTokens :: FilePath -> String -> [PosToken] -> Either TRSError SectionSet
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

pTerm :: [String] -> Parser Term
pTerm vars = do
  f <- ident
  ts <-
    fromMaybe []
      <$> (optional (parens $ sepBy1 (pTerm vars) (special ',')))

  pure (if f `elem` vars && null ts then V f else F f ts)

pRule :: [String] -> Parser Rule
pRule vars = do
  lhs <- pTerm vars
  _ <- op "->"
  rhs <- pTerm vars

  pure (lhs, rhs)

pRulesSec :: [String] -> Parser TRS
pRulesSec vars = parens $ do
  _ <- keyword "RULES"

  itemsOf (pRule vars)

pSectionSet :: Parser SectionSet
pSectionSet = do
  vars <- fromMaybe [] <$> optional pVarSec

  rs <- pRulesSec vars

  pure (SectionSet vars rs)
