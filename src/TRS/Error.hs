module TRS.Error (TRSError (..)) where

import Error

data TRSError
  = SyntaxError SrcPos String
  | Invalid String
  | UnknownVariable String
  deriving (Show)
