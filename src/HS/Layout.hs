module HS.Layout (layout) where

import HS.Lexer
import Lexer

col :: PosToken -> Int
col = spCol . ptPos

-- Assume new line as ';' hence Simple Haskell as no blocks
layout :: [PosToken] -> [PosToken]
layout ps = concat [sep p | p <- ps]
  where
    sep t
      | col t == 1 = [PosToken (ptPos t) TVEndOfStmt, t]
      | otherwise = [t]
