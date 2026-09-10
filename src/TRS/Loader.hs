module TRS.Loader (convert, fromViolation, loadTRS) where

import Control.Monad.Except (MonadError (throwError))
import Data.List (nub)
import TRS
import TRS.Check (Violation (..), checkTRS)
import TRS.Error
import TRS.Parser (parseTRS)
import TRS.Syntax (SectionSet (..))
import Term

variablesInTRS :: TRS -> [Name]
variablesInTRS trs =
  nub [x | (l, r) <- trs, t <- [l, r], x <- variables t]

substituteTRS :: TRS -> Subst -> TRS
substituteTRS trs sigma =
  [ (substitute l sigma, substitute r sigma)
  | (l, r) <- trs
  ]

-- Reify every symbol not declared in (VAR ...) as a nullary function symbol.
convert :: [String] -> TRS -> TRS
convert xs trs = substituteTRS trs sigma
  where
    declared = map packName xs
    sigma = [(x, F x []) | x <- variablesInTRS trs, not (elem x declared)]

fromViolation :: Violation -> TRSError
fromViolation v = case v of
  AmbiguousOverlap r1 r2 s t ->
    Invalid
      ( "ambiguous rules: "
          ++ showRule r1
          ++ " overlaps "
          ++ showRule r2
          ++ ", and the overlap reduces to both "
          ++ show s
          ++ " and "
          ++ show t
      )
  UnresolvedOverlap r1 r2 s t ->
    Invalid
      ( "overlapping rules: "
          ++ showRule r1
          ++ " overlaps "
          ++ showRule r2
          ++ ", and neither "
          ++ show s
          ++ " nor "
          ++ show t
          ++ " reached a normal form (is the system terminating?)"
      )
  LhsIsApplication r ->
    Invalid ("variable applied to arguments in the left-hand side of " ++ showRule r)
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
