module Suite.Decode
  ( Value (..),
    decode,
    peanoTerm,
    binTerm,
    listTerm,
    termSize,
  )
where

import HS.Name
import Source (haskellNotation)
import TRS
import TRS.Pretty (consChain, numeralOf)

data Value
  = VNat Integer
  | VBin Integer
  | VBool Bool
  | VList [Value]
  | VTuple [Value]
  | VFail
  | VOther Term
  deriving (Eq, Show)

trueNames, falseNames, failNames :: [String]
trueNames = knownNames KnTrue
falseNames = knownNames KnFalse
failNames = knownNames KnFail

oneNames, obitNames, ibitNames :: [String]
oneNames = knownNames KnBinOne
obitNames = knownNames KnBinO
ibitNames = knownNames KnBinI

isTuple :: String -> Bool
isTuple f = take 5 f == "Tuple"

decode :: Term -> Value
decode t
  | Just n <- peano t = VNat n
  | Just n <- bin t = VBin n
  | Just b <- bool t = VBool b
  | Just vs <- list t = VList vs
  | F f ts <- t, isTuple f = VTuple (map decode ts)
  | F f [] <- t, f `elem` failNames = VFail
  | otherwise = VOther t
  where
    peano = numeralOf haskellNotation

    bin (F f []) | f `elem` oneNames = Just 1
    bin (F f [u]) | f `elem` obitNames = (* 2) <$> bin u
    bin (F f [u]) | f `elem` ibitNames = (\n -> 2 * n + 1) <$> bin u
    bin _ = Nothing

    bool (F f []) | f `elem` trueNames = Just True
    bool (F f []) | f `elem` falseNames = Just False
    bool _ = Nothing

    list u = case consChain haskellNotation u of
      Just (xs, Nothing) -> Just (map decode xs)
      _ -> if isEmptyList u then Just [] else Nothing
    isEmptyList (F f []) = f `elem` knownNames KnNil
    isEmptyList _ = False

peanoTerm :: Integer -> Term
peanoTerm n
  | n <= 0 = F (knownName KnZero) []
  | otherwise = F (knownName KnSucc) [peanoTerm (n - 1)]

binTerm :: Integer -> Term
binTerm n
  | n <= 1 = F (knownName KnBinOne) []
  | even n = F (knownName KnBinO) [binTerm (n `div` 2)]
  | otherwise = F (knownName KnBinI) [binTerm (n `div` 2)]

listTerm :: [Term] -> Term
listTerm = foldr (\x xs -> F (knownName KnCons) [x, xs]) (F (knownName KnNil) [])

termSize :: Term -> Int
termSize = length . positions
