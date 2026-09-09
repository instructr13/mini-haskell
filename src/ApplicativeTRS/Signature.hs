{-# LANGUAGE InstanceSigs #-}

module ApplicativeTRS.Signature (module ApplicativeTRS.Signature) where

import ApplicativeTRS.Syntax
import Data.List (nub)

data SymKind = Constructor | Function | Variable deriving (Eq, Show)

data Signature = Signature
  { sigCons :: [(String, Int)],
    sigDefined :: [String]
  }

instance Semigroup Signature where
  Signature c1 d1 <> Signature c2 d2 = Signature (c1 <> c2) (d1 <> d2)

instance Monoid Signature where
  mempty :: Signature
  mempty = Signature [] []

classify :: Signature -> String -> SymKind
classify sig x
  | isConName x = Constructor
  | x `elem` sigDefined sig = Function
  | otherwise = Variable

mkSignature :: AppModule -> Signature
mkSignature m =
  Signature
    { sigCons = [(cdName c, cdArity c) | d <- amData m, c <- ddCons d],
      sigDefined = nub [spineHead l | (l, _) <- amRules m]
    }
