module List (replaceAt) where

replaceAt :: Int -> a -> [a] -> [a]
replaceAt _ _ [] = []
replaceAt 0 new (_ : xs) = new : xs
replaceAt i new (x : xs) = x : replaceAt (i - 1) new xs
