module ApplicativeTRS.Check (ConViolation (..), checkConstructors, fromConViolation) where

import ApplicativeTRS.Signature
import ApplicativeTRS.Syntax (isConName)
import Data.List (nub, (\\))
import TRS
import TRS.Error
import Term

data ConViolation
  = UndeclaredConstructor Rule String
  | ConstructorArityExceeded Rule String Int Int
  | LhsRootIsConstructor Rule String
  | DuplicateConstructor String
  deriving (Show, Eq)

-- Every symbol occurring in a term.
symbols :: Term -> [(String, Int)]
symbols (V _) = []
symbols (F f ts) = (f, length ts) : concat [symbols t | t <- ts]

checkDuplicateConstructor :: Signature -> [ConViolation]
checkDuplicateConstructor sig = [DuplicateConstructor c | c <- nub (names \\ nub names)]
  where
    names = map fst (sigCons sig)

-- bad: Cons x xs -> x   (a constructor is not a defined symbol)
checkLhsRootIsConstructor :: Rule -> [ConViolation]
checkLhsRootIsConstructor rule@(F c _, _) | isConName c = [LhsRootIsConstructor rule c]
checkLhsRootIsConstructor _ = []

checkConstructorUse :: Signature -> Rule -> [ConViolation]
checkConstructorUse sig rule@(l, r) =
  [ v
  | (f, n) <- symbols l ++ symbols r,
    isConName f,
    v <- case lookup f (sigCons sig) of
      Nothing -> [UndeclaredConstructor rule f]
      Just arity | n > arity -> [ConstructorArityExceeded rule f arity n]
      _ -> []
  ]

checkConstructors :: Signature -> TRS -> [ConViolation]
checkConstructors sig trs =
  checkDuplicateConstructor sig
    ++ concat [checkLhsRootIsConstructor rule ++ checkConstructorUse sig rule | rule <- trs]

fromConViolation :: ConViolation -> TRSError
fromConViolation v = case v of
  UndeclaredConstructor rule c ->
    Invalid
      ( "undeclared constructor "
          ++ c
          ++ " (missing a data declaration, or did you mean a lowercase variable?) in "
          ++ showRule rule
      )
  ConstructorArityExceeded rule c declared actual ->
    Invalid
      ( "constructor "
          ++ c
          ++ " is applied to "
          ++ show actual
          ++ " argument(s) but declared with arity "
          ++ show declared
          ++ " in "
          ++ showRule rule
      )
  LhsRootIsConstructor rule c ->
    Invalid ("constructor " ++ c ++ " cannot be redefined as a rule head: " ++ showRule rule)
  DuplicateConstructor c ->
    Invalid ("constructor " ++ c ++ " is declared more than once")
