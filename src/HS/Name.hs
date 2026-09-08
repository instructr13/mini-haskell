module HS.Name (module HS.Name) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import TRS

newtype VarName = VarName {unVarName :: String} deriving (Eq, Ord, Show)

newtype ConName = ConName {unConName :: String} deriving (Eq, Ord, Show)

newtype TyConName = TyConName {unTyConName :: String} deriving (Eq, Ord, Show)

data SymKind
  = ConSym TyConName
  | FunSym

data SymInfo = SymInfo {symArity :: !Int, symKind :: SymKind}

type Signature = Map String SymInfo

data Known = Known
  { knTrue :: ConName,
    knFalse :: ConName,
    knNil :: ConName,
    knCons :: ConName,
    knTuple :: Int -> ConName,
    knZero :: ConName,
    knSucc :: ConName,
    knFail :: ConName
  }

symbolChars :: String
symbolChars = "!#$%&*+./<=>?@\\^|-~:"

isOperatorName :: String -> Bool
isOperatorName "" = False
isOperatorName s = all (`elem` symbolChars) s

isConSym :: SymInfo -> Bool
isConSym i = case symKind i of
  ConSym _ -> True
  FunSym -> False

tupleConName :: Int -> String
tupleConName n = "Tuple" ++ show n

defaultKnown :: Known
defaultKnown =
  Known
    { knTrue = ConName "True",
      knFalse = ConName "False",
      knNil = ConName "Nil",
      knCons = ConName "Cons",
      knTuple = ConName . tupleConName,
      knZero = ConName "Zero",
      knSucc = ConName "Succ",
      knFail = ConName "Fail"
    }

resolveKnown :: Signature -> Either String Known
resolveKnown sig =
  defaultKnown
    <$ mapM_
      require
      [ (knTrue defaultKnown, 0),
        (knFalse defaultKnown, 0),
        (knNil defaultKnown, 0),
        (knCons defaultKnown, 2),
        (knZero defaultKnown, 0),
        (knSucc defaultKnown, 1),
        (knFail defaultKnown, 0)
      ]
  where
    require (ConName name, n) = case Map.lookup name sig of
      Just i | isConSym i, symArity i == n -> Right ()
      _ -> Left name

signatures :: Term -> Signature
signatures (V _) = Map.empty
signatures (F f ts) = Map.insert f (SymInfo {symArity = length ts, symKind = FunSym}) (Map.unions [signatures t | t <- ts])
