module HS.Lexer (Token (..), PosToken (..), showToken, unvirtual, lexHS, tokenWidth) where

import Data.List (sortBy)
import Error
import Lexer
import Text.Megaparsec hiding (ParseError, Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Refer docs/TRS.md for EBNF

data Token
  = TIdent String -- @x@
  | TKeyword String -- @data@
  | TOp String
  | TNumLiteral Int -- Number literals
  | TSpecial Char -- @( ) [ ] , ;@
  | TWild -- @_@
  | TVEndOfStmt
  deriving (Eq, Ord, Show)

showToken :: Token -> String
showToken (TIdent s) = s
showToken (TKeyword s) = s
showToken (TOp s) = s
showToken (TNumLiteral n) = show n
showToken (TSpecial c) = [c]
showToken TWild = "_"
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
sc = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keywords :: [String]
keywords = ["data"]

ops :: [String]
ops =
  sortBy
    (flip compare)
    [ "=", -- Any declaration
      "|", -- Data alternative

      --- General operators

      -- Priority 9
      ".", -- Function composition (right)

      -- Priority 7
      "*", -- mul (left)

      -- Priority 6
      "+", -- add (left)
      "-", -- sub (left)

      -- Priority 5
      ":", -- cons (right)
      "++", -- append (right)

      -- Priority 4
      "==", -- eq
      "/=", -- neq
      "<=", -- leq
      "<", -- lt
      ">=", -- geq
      ">", -- gt

      -- Priority 3
      "&&", -- and

      -- Priority 2
      "||" -- or
    ]

specialChars :: String
specialChars = "()[],;"

lexHS :: FilePath -> String -> Either ParseError [PosToken]
lexHS = parse (sc *> many pTokenWithPos <* eof)

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
      pWild,
      pOp,
      pLiteral,
      pSpecial
    ]
    <?> "token"

pKeywordOrIdent :: Parser Token
pKeywordOrIdent = try $ do
  first <- letterChar
  rest <- many (alphaNumChar <|> char '_')

  let word = first : rest

  pure (if word `elem` keywords then TKeyword word else TIdent word)

pWild :: Parser Token
pWild = do
  c <- TWild <$ char '_'

  notFollowedBy (alphaNumChar <|> char '_')

  pure c

pOp :: Parser Token
pOp = TOp <$> choice (map string ops)

pLiteral :: Parser Token
pLiteral =
  choice
    [ TNumLiteral <$> L.decimal
    ]

pSpecial :: Parser Token
pSpecial = TSpecial <$> oneOf specialChars

tokenWidth :: Token -> Int
tokenWidth t
  | isVirtual t = 0
  | otherwise = length (showToken t)
