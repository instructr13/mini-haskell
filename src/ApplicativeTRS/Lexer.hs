module ApplicativeTRS.Lexer (Token (..), PosToken (..), showToken, unvirtual, lexApplicativeTRS, tokenWidth) where

import Data.List (sortBy)
import Error
import Lexer
import Text.Megaparsec hiding (ParseError, Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Refer docs/TRS.md for EBNF

data Token
  = TIdent String -- @x@
  | TKeyword String -- @RULES@, @DATA@
  | TOp String -- @->@
  | TNumLiteral Int -- Number literals
  | TSpecial Char -- @( ) [ ] , ;@
  | TVOpenBlock
  | TVCloseBlock
  | TVEndOfStmt
  deriving (Eq, Ord, Show)

showToken :: Token -> String
showToken (TIdent s) = s
showToken (TKeyword s) = s
showToken (TOp s) = s
showToken (TNumLiteral n) = show n
showToken (TSpecial c) = [c]
showToken TVOpenBlock = "("
showToken TVCloseBlock = ")"
showToken TVEndOfStmt = "\n"

data PosToken = PosToken
  { ptPos :: !SrcPos,
    ptTok :: !Token
  }
  deriving (Eq, Ord, Show)

isVirtual :: Token -> Bool
isVirtual t = t `elem` [TVEndOfStmt]

unvirtual :: Token -> Token
unvirtual TVOpenBlock = TSpecial '('
unvirtual TVCloseBlock = TSpecial ')'
unvirtual TVEndOfStmt = TSpecial ';'
unvirtual t = t

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

keywords :: [String]
keywords = ["RULES", "DATA"]

ops :: [String]
ops =
  sortBy
    (flip compare)
    [ "->", -- Rule assoc
      "=", -- Data declaration
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

lexApplicativeTRS :: FilePath -> String -> Either ParseError [PosToken]
lexApplicativeTRS = parse (sc *> many pTokenWithPos <* eof)

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
