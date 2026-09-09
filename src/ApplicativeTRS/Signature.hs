module ApplicativeTRS.Signature (module ApplicativeTRS.Signature) where

import ApplicativeTRS.Syntax
import Data.List (nub)

data SymKind = Constructor | Function | Variable deriving (Eq, Show)

data Signature = Signature
  { sigCons :: [(String, Int)],
    sigDefined :: [String]
  }

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
