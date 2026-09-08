module HS.Name (module HS.Name) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import TRS

newtype VarName = VarName {unVarName :: String} deriving (Eq, Ord, Show)

newtype ConName = ConName {unConName :: String} deriving (Eq, Ord, Show)

newtype TyConName = TyConName {unTyConName :: String} deriving (Eq, Ord, Show)

data SymKind
  = ConSym TyConName
  | FunSym
  deriving (Eq, Show)

data SymInfo = SymInfo {symArity :: !Int, symKind :: SymKind}
  deriving (Eq, Show)

type Signature = Map String SymInfo

-- The constructors and functions the compiler must be able to name itself.
-- Tuples are a family rather than a single name, so they are not listed here;
-- 'tupleConName' is their single source of truth.
data KnownName
  = KnTrue
  | KnFalse
  | KnNil
  | KnCons
  | KnZero
  | KnSucc
  | KnBinOne
  | KnBinO
  | KnBinI
  | KnChar
  | KnFail
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Total over KnownName, so -Wincomplete-patterns turns "a wired-in name was
-- added but not given a spelling" into a compile error and validation can
-- never drift from the definition.
--
-- Each name has a list of accepted spellings rather than one, because the
-- same wired-in role is written differently in different sources (Zero/Succ
-- in a spelled-out Prelude, Z/S in the terser ones). The first spelling that
-- is declared with the right arity wins; the first of the list is the one
-- error messages ask for.
knownSpec :: KnownName -> ([String], Int)
knownSpec k = case k of
  KnTrue -> (["True"], 0)
  KnFalse -> (["False"], 0)
  KnNil -> (["Nil"], 0)
  KnCons -> (["Cons"], 2)
  KnZero -> (["Zero", "Z"], 0)
  KnSucc -> (["Succ", "S"], 1)
  KnBinOne -> (["One"], 0)
  KnBinO -> (["O"], 1)
  KnBinI -> (["I"], 1)
  KnChar -> (["MkChar", "Char"], 1)
  KnFail -> (["Fail", "PatternMatchFail"], 0)

knownNames :: KnownName -> [String]
knownNames = fst . knownSpec

knownName :: KnownName -> String
knownName k = case knownNames k of
  (n : _) -> n
  [] -> show k

knownArity :: KnownName -> Int
knownArity = snd . knownSpec

data NumericRep = NumPeano | NumBinary
  deriving (Eq, Show)

numericName :: NumericRep -> String
numericName NumPeano = "peano"
numericName NumBinary = "binary"

numericOfName :: String -> Maybe NumericRep
numericOfName s = lookup s [(numericName r, r) | r <- [NumPeano, NumBinary]]

symbolChars :: String
symbolChars = "!#$%&*+./<=>?@\\^|-~:"

-- The compiler marks every name it generates with '#', so stripping the
-- suffix recovers the source name a generated one came from. Used to keep
-- generated names readable rather than stacking suffixes.
generatedMark :: Char
generatedMark = '#'

isGeneratedName :: String -> Bool
isGeneratedName = elem generatedMark

nameBase :: String -> String
nameBase v = case break (== generatedMark) v of
  (b, _ : _) | not (null b) -> b
  _ -> v

-- Haskell 2010 2.4: an operator whose first character is ':' is a constructor
-- operator, every other one is a variable operator.
isConOperator :: String -> Bool
isConOperator (':' : _) = True
isConOperator _ = False

isOperatorName :: String -> Bool
isOperatorName "" = False
isOperatorName s = all (`elem` symbolChars) s

isConSym :: SymInfo -> Bool
isConSym i = case symKind i of
  ConSym _ -> True
  FunSym -> False

constructorSymbols :: Signature -> Set String
constructorSymbols = Map.keysSet . Map.filter isConSym

functionSymbols :: Signature -> Set String
functionSymbols = Map.keysSet . Map.filter (not . isConSym)

tupleConName :: Int -> ConName
tupleConName n = ConName ("Tuple" ++ show n)

unknownTyCon :: TyConName
unknownTyCon = TyConName "?"

-- Best-effort signature for a TRS read from a .trs file, where there are no
-- data declarations to say which symbols are constructors.
signatureFromTRS :: TRS -> Signature
signatureFromTRS trs =
  Map.fromListWith
    keepFirst
    [ (f, SymInfo n (kindOf f))
    | (l, r) <- trs,
      t <- [l, r],
      (f, n) <- symbolOccurrences t
    ]
  where
    defined = definedSymbols trs
    kindOf f
      | Set.member f defined = FunSym
      | otherwise = ConSym unknownTyCon
    keepFirst _new old = old
