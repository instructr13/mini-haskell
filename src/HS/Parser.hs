module HS.Parser (parseHS) where

import Control.Monad.Combinators.Expr
import Data.Void
import HS.Desugar
import HS.Layout (layout)
import HS.Lexer
import HS.Operator
import HS.Syntax
import HS.TokenStream
import TRS.Error
import Text.Megaparsec hiding (Token)
import Util

-- Built from HS.Operator, the single source of truth for the operator set.
operatorTable :: [[Operator Parser SExpr]]
operatorTable =
  [InfixL (SEApp <$ op ".")]
    : [[entry o | o <- g] | g <- operatorsByPrec]
  where
    entry :: OpSpec -> Operator Parser SExpr
    entry o = infixOf (opAssoc o) (mkBinOp (opFunctor o) <$ op (opSymbol o))

    infixOf :: Assoc -> Parser (SExpr -> SExpr -> SExpr) -> Operator Parser SExpr
    infixOf AssocLeft = InfixL
    infixOf AssocNone = InfixN
    infixOf AssocRight = InfixR

    mkBinOp :: String -> SExpr -> SExpr -> SExpr
    mkBinOp name l r = SEApp (SEApp (SEIdent name) l) r

parseHS :: FilePath -> String -> Either TRSError Module
parseHS file src = do
  toks <- mapLeft syntaxError (lexHS file src)

  parseTokens file src (layout toks)

syntaxError :: (TraversableStream s, VisualStream s) => ParseErrorBundle s Void -> TRSError
syntaxError e = SyntaxError (bundleErrorPos e) (errorBundlePretty e)

parseTokens :: FilePath -> String -> [PosToken] -> Either TRSError Module
parseTokens file src toks = mapLeft syntaxError m
  where
    m = parse (pModule <* eof) file (tokenStream src toks)

pSimpleExpression :: Parser SExpr
pSimpleExpression = SEIdent <$> ident <|> pDesugared pTerm <|> parens pTerm

pApp :: Parser SExpr
pApp = foldl SEApp <$> pSimpleExpression <*> many pSimpleExpression

pTerm :: Parser SExpr
pTerm = makeExprParser pApp operatorTable

pAtomicPattern :: Parser Pat
pAtomicPattern =
  PVar
    <$> varIdent
      <|> PCon
    <$> conIdent
    <*> pure []
      <|> pDesugared pPattern
      <|> parens pPattern

pConPattern :: Parser Pat
pConPattern = PCon <$> conIdent <*> many (pAtomicPattern) <|> pAtomicPattern

pPattern :: Parser Pat
pPattern = makeExprParser pConPattern [[InfixR (mkCons <$ op ":")]]
  where
    mkCons x xs = PCon "Cons" [x, xs]

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

pDataDecl :: Parser DataDecl
pDataDecl = do
  _ <- keyword "data"

  name <- conIdent
  _ <- many varIdent
  _ <- op "="

  DataDecl name <$> sepBy1 pConDecl (op "|")

pRuleDecl :: Parser RuleDecl
pRuleDecl = RuleDecl <$> varIdent <*> many pAtomicPattern <* op "=" <*> pTerm

pDecl :: Parser Decl
pDecl = DData <$> pDataDecl <|> DRule <$> pRuleDecl

pModule :: Parser Module
pModule = mkModule <$> itemsOf pDecl
