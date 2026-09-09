module ApplicativeTRS.Parser (parseApplicativeTRS) where

import ApplicativeTRS.Desugar
import ApplicativeTRS.Lexer
import ApplicativeTRS.Syntax
import ApplicativeTRS.TokenStream
import Control.Monad.Combinators.Expr
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Void
import TRS.Error
import Text.Megaparsec hiding (Token)
import Util

-- See op of ApplicativeTRS.Lexer for definition
operatorTable :: [[Operator Parser SExpr]]
operatorTable =
  [ [ InfixL (mkBinOp "mul" <$ op "*")
    ],
    [ InfixL (mkBinOp "add" <$ op "+"),
      InfixL (mkBinOp "sub" <$ op "-")
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

pVarSec :: Parser [String]
pVarSec = parens $ do
  _ <- keyword "VAR"

  nub <$> many ident

pSimpleExpression :: Parser SExpr
pSimpleExpression = SEIdent <$> ident <|> pDesugarExpression pTerm <|> parens pTerm

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
