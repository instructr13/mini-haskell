module TRS.Error (TRSError (..)) where

import Lexer

data TRSError
  = SyntaxError SrcPos String
  | ReservedSymbol String
  | Invalid String
  | UnknownVariable String
  deriving (Show)
