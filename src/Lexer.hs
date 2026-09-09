module Lexer (module Lexer) where

import Data.Void
import Text.Megaparsec

data SrcPos = SrcPos {spLine :: !Int, spCol :: !Int}
  deriving (Eq, Ord, Show)

type Parser = Parsec Void String

-- get position using megaparsec
currentPos :: Parser SrcPos
currentPos = do
  p <- getSourcePos
  pure (SrcPos (unPos (sourceLine p)) (unPos (sourceColumn p)))
