module ApplicativeTRS.Parser (parseApplicativeTRS) where

import ApplicativeTRS.Lexer
import ApplicativeTRS.Special.Peano (toPeanoSExpr)
import ApplicativeTRS.Syntax
import ApplicativeTRS.TokenStream
import Control.Monad.Combinators.Expr
import Data.List (nub)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Data.Void
import TRS.Error
import Text.Megaparsec hiding (Token)
import Util

type Parser = Parsec Void TokenStream

-- See op of ApplicativeTRS.Lexer for definition
operatorTable :: [[Operator Parser SExpr]]
operatorTable =
  [ [ InfixL (mkBinOp "add" <$ op "+")
    ],
    [ InfixR (mkBinOp "cons" <$ op ":"),
      InfixR (mkBinOp "append" <$ op "++")
    ],
    [ InfixN (mkBinOp "eq" <$ op "=="),
      InfixN (mkBinOp "neq" <$ op "/="),
      InfixN (mkBinOp "leq" <$ op "<="),
      InfixN (mkBinOp "lt" <$ op "<"),
      InfixN (mkBinOp "geq" <$ op ">="),
      InfixN (mkBinOp "gt" <$ op ">"),
      InfixN (mkBinOp "neq" <$ op "/=")
    ],
    [ InfixR (mkBinOp "and" <$ op "&&")
    ],
    [ InfixR (mkBinOp "or" <$ op "||")
    ]
  ]
  where
    mkBinOp :: String -> SExpr -> SExpr -> SExpr
    mkBinOp name l r = SEApp (SEApp (SEIdent name) l) r

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

numLiteral :: Parser Int
numLiteral = satisfyT check <?> "numbers"
  where
    check :: Token -> Maybe Int
    check (TNumLiteral n) = Just n
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

pPeanoNum :: Parser SExpr
pPeanoNum = toPeanoSExpr <$> numLiteral

pSimpleExpression :: Parser SExpr
pSimpleExpression = SEIdent <$> ident <|> pPeanoNum <|> parens pTerm

pApplication :: Parser SExpr
pApplication = foldl SEApp <$> pSimpleExpression <*> many pSimpleExpression

pTerm :: Parser SExpr
pTerm = makeExprParser pApplication operatorTable

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
