module Util (module Util) where

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)

setEqual :: (Eq a) => [a] -> [a] -> Bool
setEqual xs ys = and [x `elem` ys | x <- xs] && and [y `elem` xs | y <- ys]
