module ApplicativeTRS.Parser (parseApplicativeTRS) where

import ApplicativeTRS.Desugar
import ApplicativeTRS.Lexer
import ApplicativeTRS.Syntax
import ApplicativeTRS.TokenStream
import Control.Monad.Combinators.Expr
import Data.Void
import TRS.Error
import Text.Megaparsec hiding (Token)
import Util

-- See op of ApplicativeTRS.Lexer for definition
operatorTable :: [[Operator Parser SExpr]]
operatorTable =
  [ [ InfixR (mkBinOp "compose" <$ op ".")
    ],
    [ InfixL (mkBinOp "mul" <$ op "*")
    ],
    [ InfixL (mkBinOp "add" <$ op "+"),
      InfixL (mkBinOp "sub" <$ op "-")
    ],
    [ InfixR (mkBinOp "Cons" <$ op ":"), -- : is only the constructor "Cons"
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

parseApplicativeTRS :: FilePath -> String -> Either TRSError AppModule
parseApplicativeTRS file src = do
  toks <- mapLeft syntaxError (lexApplicativeTRS file src)

  parseTokens file src toks

syntaxError :: (TraversableStream s, VisualStream s) => ParseErrorBundle s Void -> TRSError
syntaxError e = SyntaxError (bundleErrorPos e) (errorBundlePretty e)

parseTokens :: FilePath -> String -> [PosToken] -> Either TRSError AppModule
parseTokens file src toks = mapLeft syntaxError sectionSet
  where
    sectionSet = parse (pSectionSet <* eof) file (tokenStream src toks)

pSimpleExpression :: Parser SExpr
pSimpleExpression = SEIdent <$> ident <|> pDesugarExpression pTerm <|> parens pTerm

pApp :: Parser SExpr
pApp = foldl SEApp <$> pSimpleExpression <*> many pSimpleExpression

pTerm :: Parser SExpr
pTerm = makeExprParser pApp operatorTable

pRule :: Parser AppRule
pRule = do
  lhs <- pTerm
  _ <- op "->"
  rhs <- pTerm
  _ <- semi

  pure (lhs, rhs)

pTypeAtom :: Parser ()
pTypeAtom =
  ()
    <$ conIdent
      <|> ()
    <$ varIdent
      <|> ()
    <$ parens (conIdent *> many pTypeAtom)

pConDecl :: Parser ConDecl
pConDecl = ConDecl <$> conIdent <*> (length <$> many pTypeAtom)

pDataSec :: Parser DataDecl
pDataSec = parens $ do
  _ <- keyword "DATA"

  name <- conIdent
  _ <- many varIdent
  _ <- op "="

  DataDecl name <$> sepBy1 pConDecl (op "|")

pRulesSec :: Parser [AppRule]
pRulesSec = parens $ do
  _ <- keyword "RULES"

  itemsOf pRule

pSectionSet :: Parser AppModule
pSectionSet = do
  ds <- many (try pDataSec)
  rs <- option [] pRulesSec

  pure (AppModule {amData = ds, amRules = rs})
