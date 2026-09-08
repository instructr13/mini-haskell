module Error (module Error) where

import Data.Void
import Text.Megaparsec

data SrcPos = SrcPos {spLine :: !Int, spCol :: !Int}
  deriving (Eq, Ord, Show)

type Parser = Parsec Void String

type ParseError = ParseErrorBundle String Void
