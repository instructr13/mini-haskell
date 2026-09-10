module Wild (wildVar, isWildVar) where

import Data.Char

wildVar :: Int -> String
wildVar n = "_#" ++ show n

isWildVar :: String -> Bool
-- "_#" ++ some string
isWildVar ('_' : '#' : ds@(_ : _)) = all isDigit ds
isWildVar _ = False
