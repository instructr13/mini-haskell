module HS.Check (DeclViolation (..), checkModule, fromDeclViolation) where

import ApplicativeTRS.Signature
import Data.List (intercalate, nub)
import HS.Syntax
import TRS.Error

data DeclViolation
  = InconsistentArity String [Int]
  | UnsaturatedConstructor String String Int Int
  deriving (Show, Eq)

-- bad: length Nil        = 0
--      length (x : xs) y = 1 + length xs y
checkInconsistentArity :: Module -> [DeclViolation]
checkInconsistentArity m =
  [ InconsistentArity f arities
  | f <- nub [rdName r | r <- mRules m],
    let arities = nub [length (rdPats r) | r <- mRules m, rdName r == f],
    length arities > 1
  ]

conPats :: Pat -> [(String, Int)]
conPats (PVar _) = []
conPats (PCon c ps) = (c, length ps) : concat [conPats p | p <- ps]
conPats PWild = []

-- bad: head (Cons x) = x     (Cons is declared with arity 2)
checkUnsaturatedConstructor :: Signature -> Module -> [DeclViolation]
checkUnsaturatedConstructor sig m =
  [ UnsaturatedConstructor (rdName r) c declared n
  | r <- mRules m,
    (c, n) <- concat [conPats p | p <- rdPats r],
    Just declared <- [lookup c (sigCons sig)],
    n /= declared
  ]

checkModule :: Signature -> Module -> [DeclViolation]
checkModule sig m = checkInconsistentArity m ++ checkUnsaturatedConstructor sig m

fromDeclViolation :: DeclViolation -> TRSError
fromDeclViolation v = case v of
  InconsistentArity f arities ->
    Invalid
      ( "rules for "
          ++ f
          ++ " have different numbers of arguments ("
          ++ intercalate ", " (map show arities)
          ++ ")"
      )
  UnsaturatedConstructor f c declared actual ->
    Invalid
      ( "constructor "
          ++ c
          ++ " is matched against "
          ++ show actual
          ++ " argument(s) but declared with arity "
          ++ show declared
          ++ " in an equation for "
          ++ f
      )
