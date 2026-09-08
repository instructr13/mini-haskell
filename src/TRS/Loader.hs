module TRS.Loader (loadTRS) where

import Control.Monad.Except (MonadError (throwError))
import Data.List (nub)
import TRS
import TRS.Check (Violation (..), checkTRS)
import TRS.Error
import TRS.Parser (parseTRS)
import TRS.Syntax (SectionSet (..))

variablesInTRS :: TRS -> [String]
variablesInTRS trs =
  nub [x | (l, r) <- trs, t <- [l, r], x <- variables t]

substituteTRS :: TRS -> Subst -> TRS
substituteTRS trs sigma =
  [ (substitute l sigma, substitute r sigma)
  | (l, r) <- trs
  ]

convert :: [String] -> TRS -> TRS
convert xs trs = substituteTRS trs sigma
  where
    sigma = [(x, F x []) | x <- variablesInTRS trs, not (elem x xs)]

fromViolation :: Violation -> TRSError
fromViolation v = case v of
  RootOverlap _ _ -> error "BUG: root overlap (possibly implementation mistake of match)"
  NonLeftLinear r x ->
    Invalid ("non-linear pattern (" ++ x ++ ") in " ++ showRule r)
  UnboundRhsVar _ x -> UnknownVariable x
  LhsIsVariable r -> Invalid ("variable left-hand side: " ++ showRule r)
  NotConstructorSystem r ->
    Invalid ("not a constructor system: " ++ showRule r)

loadTRS :: FilePath -> String -> Either TRSError TRS
loadTRS file src = do
  sectionSet <- parseTRS file src

  let trs = convert (ssVars sectionSet) (ssRules sectionSet)

  case checkTRS trs of
    [] -> pure trs
    (v : _) -> throwError (fromViolation v)
