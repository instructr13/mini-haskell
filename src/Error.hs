module Error (module Error) where

import Data.Void
import Text.Megaparsec

type ParseError = ParseErrorBundle String Void
