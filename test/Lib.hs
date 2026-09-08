module Lib (module Lib) where

import TRS
import TRS.Parser

trs :: String -> TRS
trs = either (error . show) id . readTRS

term :: [String] -> String -> Term
term vs s = case readTRS ("(VAR" ++ unwords vs ++ ") (RULES" ++ s ++ " -> " ++ s ++ ")") of
  Left e -> error (show e)
  Right ((l, _) : _) -> l
  Right [] -> error "term: empty"

runMain :: String -> String
runMain src = show (nf (trs src) (F "main" []))

expect :: (Eq a, Show a) => String -> a -> a -> IO ()
expect name got want
  | got == want = putStrLn ("ok   " ++ name)
  | otherwise = putStrLn ("FAIL " ++ name ++ "\n  got: " ++ show got ++ "\n  want: " ++ show want)
