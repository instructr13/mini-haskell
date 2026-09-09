module Lexer (module Lexer) where

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data SrcPos = SrcPos {spLine :: !Int, spCol :: !Int}
  deriving (Eq, Ord, Show)

type Parser = Parsec Void String

-- get position using megaparsec
currentPos :: Parser SrcPos
currentPos = do
  p <- getSourcePos
  pure (SrcPos (unPos (sourceLine p)) (unPos (sourceColumn p)))

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
