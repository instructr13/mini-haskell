module TRS.Special.Peano (toPeano) where

import Term

toPeano :: Int -> Term
toPeano 0 = F "0" []
toPeano i
  | i < 0 = error "toPeano: value must be a positive integer"
  | otherwise = F "s" [toPeano (i - 1)]
