module Util (module Util) where

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f = either Left (Right . f)
