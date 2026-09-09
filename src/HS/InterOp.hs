module HS.InterOp (appTRSToHS) where

import ApplicativeTRS.Syntax (AppModule (..))
import qualified ApplicativeTRS.Syntax as AS
import HS.Syntax

asConsToHSCons :: [AS.ConDecl] -> [ConDecl]
asConsToHSCons ascs = [ConDecl (AS.cdName asc) (AS.cdArity asc) | asc <- ascs]

asSExprToHSExpr :: AS.SExpr -> SExpr
asSExprToHSExpr (AS.SEIdent x) = SEIdent x
asSExprToHSExpr (AS.SEApp f g) = SEApp (asSExprToHSExpr f) (asSExprToHSExpr g)

asSExprToPat :: AS.SExpr -> Pat
asSExprToPat e = case AS.spine e of
  (x, []) | not (AS.isConName x) -> PVar x
  (c, args) | AS.isConName c -> PCon c [asSExprToPat a | a <- args]
  _ -> error ("asSExprToPat: not a pattern: " ++ show e)

asLHSToHSLHS :: AS.SExpr -> (String, [Pat])
asLHSToHSLHS l = case AS.spine l of
  (f, args) | not (AS.isConName f) -> (f, [asSExprToPat a | a <- args])
  _ -> error ("asLHSToHSLHS: not a rule head: " ++ show l)

appTRSToHS :: AppModule -> Module
appTRSToHS am =
  Module
    [DataDecl (AS.ddName d) (asConsToHSCons (AS.ddCons d)) | d <- amData am]
    [RuleDecl f ps (asSExprToHSExpr r) | (l, r) <- amRules am, let (f, ps) = asLHSToHSLHS l]
