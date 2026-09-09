module TRS.Lexer (Token (..), PosToken (..), showToken, unvirtual, lexTRS, tokenWidth) where

import Error
import Lexer
import Text.Megaparsec hiding (ParseError, Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Refer docs/TRS.md for EBNF

data Token
  = TIdent String -- @x@
  | TKeyword String -- @VAR@, @RULES@
  | TOp String -- @->@
  | TSpecial Char -- @( ) , ;@
  | TVEndOfStmt
  deriving (Eq, Ord, Show)

showToken :: Token -> String
showToken (TIdent s) = s
showToken (TKeyword s) = s
showToken (TOp s) = s
showToken (TSpecial c) = [c]
showToken TVEndOfStmt = "\n"

data PosToken = PosToken
  { ptPos :: !SrcPos,
    ptTok :: !Token
  }
  deriving (Eq, Ord, Show)

isVirtual :: Token -> Bool
isVirtual t = t `elem` [TVEndOfStmt]

unvirtual :: Token -> Token
unvirtual TVEndOfStmt = TSpecial ';'
unvirtual t = t

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keywords :: [String]
keywords = ["VAR", "RULES"]

ops :: [String]
ops = ["->"]

specialChars :: String
specialChars = "(),;"

lexTRS :: FilePath -> String -> Either ParseError [PosToken]
lexTRS = parse (sc *> many pTokenWithPos <* eof)

pTokenWithPos :: Parser PosToken
pTokenWithPos = do
  p <- currentPos
  t <- lexeme $ pToken

  sc

  pure (PosToken p t)

pToken :: Parser Token
pToken =
  choice
    [ pKeywordOrIdent,
      pOp,
      pSpecial
    ]
    <?> "token"

pKeywordOrIdent :: Parser Token
pKeywordOrIdent = try $ do
  word <- some (alphaNumChar <|> char '_')

  pure (if word `elem` keywords then TKeyword word else TIdent word)

pOp :: Parser Token
pOp = TOp <$> choice (map string ops)

pSpecial :: Parser Token
pSpecial = TSpecial <$> oneOf specialChars

tokenWidth :: Token -> Int
tokenWidth t
  | isVirtual t = 0
  | otherwise = length (showToken t)
