{-# LANGUAGE OverloadedStrings #-}

module HS.Pretty
  ( prettyModule,
    prettyDecl,
    prettyExpr,
    prettyPat,
    prettyLiteral,
    Prec (..),
  )
where

import HS.Name
import HS.Syntax
import Prettyprinter
import Render

-- Joins with a separator placed between the items.
joinWith :: Doc Ann -> [Doc Ann] -> Doc Ann
joinWith between = concatWith (\a b -> a <> between <> b)

prettyModule :: Module -> Doc Ann
prettyModule ds = joinWith (space <> punct ";" <> hardline) (concatMap declLines ds)

declLines :: Decl -> [Doc Ann]
declLines (DFun n cs) = map (prettyClause n) cs
declLines (DData t vs cs) = [dataDoc t vs cs]
declLines (DFixity a n ops) = [fixityDoc a n ops]

prettyDecl :: Decl -> Doc Ann
prettyDecl = vsep . declLines

dataDoc :: TyConName -> [VarName] -> [(ConName, Int)] -> Doc Ann
dataDoc t vs cs =
  hsep $
    [keyword "data", conName (pretty (unTyConName t))]
      ++ map (varName . pretty . unVarName) vs
      ++ [operator "=", joinWith (space <> punct "|" <> space) (map conDecl cs)]
  where
    conDecl (c, n) = hsep (conName (pretty (unConName c)) : replicate n (varName "a"))

fixityDoc :: Assoc -> Int -> [String] -> Doc Ann
fixityDoc a n ops =
  hsep [keyword (pretty (assocKeyword a)), number n, joinWith (punct "," <> space) (map opDoc ops)]

prettyClause :: VarName -> Clause -> Doc Ann
prettyClause n (Clause ps rhs ws) =
  nest 2 $
    hsep (nameDoc (unVarName n) : map (prettyPat PArg) ps)
      <> prettyRhs (operator "=") rhs
      <> prettyWhere ws

prettyRhs :: Doc Ann -> Rhs -> Doc Ann
prettyRhs eq (Plain e) = space <> eq <+> prettyExpr PTop e
prettyRhs eq (Guarded gs) =
  hcat
    [ space <> punct "|" <+> prettyExpr PTop g <+> eq <+> prettyExpr PTop e
    | (g, e) <- gs
    ]

prettyWhere :: [Decl] -> Doc Ann
prettyWhere [] = mempty
prettyWhere ds = space <> keyword "where" <+> declBlock ds

declBlock :: [Decl] -> Doc Ann
declBlock = braceBlock . concatMap declLines

braceBlock :: [Doc Ann] -> Doc Ann
braceBlock ds =
  punct "{" <+> joinWith (punct ";" <> space) ds <+> punct "}"

data Prec = PTop | POp | PApply | PArg deriving (Eq, Ord, Show)

prettyExpr :: Prec -> Expr -> Doc Ann
prettyExpr ctx e = case e of
  EVar v -> nameDoc (unVarName v)
  ECon c -> conDoc (unConName c)
  ELiteral l -> prettyLiteral l
  EList es -> bracketed (commaSep (map (prettyExpr PTop) es))
  ETuple es -> parenthesized (commaSep (map (prettyExpr PTop) es))
  ESectionL op x -> parenthesized (prettyExpr PApply x <+> opDoc op)
  ESectionR op x -> parenthesized (opDoc op <+> prettyExpr PApply x)
  EApply _ _ ->
    let (f, as) = appSpine e
     in wrap PApply (hsep (prettyExpr PApply f : map (prettyExpr PArg) as))
  EOpChain e0 rs ->
    wrap POp $
      hsep $
        prettyExpr PApply e0
          : concat [[opDoc op, prettyExpr PApply x] | (op, x) <- rs]
  ELambda ps b ->
    wrap
      PTop
      ( operator "\\"
          <> hsep (map (prettyPat PArg) ps)
            <+> operator "->"
            <+> prettyExpr PTop b
      )
  ELet ds b ->
    wrap
      PTop
      ( keyword "let"
          <+> declBlock ds
          <+> keyword "in"
          <+> prettyExpr PTop b
      )
  EIf c t f ->
    wrap
      PTop
      ( keyword "if"
          <+> prettyExpr PTop c
          <+> keyword "then"
          <+> prettyExpr PTop t
          <+> keyword "else"
          <+> prettyExpr PTop f
      )
  ECase s alts ->
    wrap
      PTop
      ( keyword "case"
          <+> prettyExpr PTop s
          <+> keyword "of"
          <+> braceBlock (map prettyAlt alts)
      )
  where
    wrap = parenWhen ctx

prettyAlt :: Alt -> Doc Ann
prettyAlt (Alt p rhs ws) =
  prettyPat PTop p <> prettyRhs (operator "->") rhs <> prettyWhere ws

prettyPat :: Prec -> Pat -> Doc Ann
prettyPat ctx p = case p of
  PVar v -> nameDoc (unVarName v)
  PWild -> varName "_"
  PLiteral l -> prettyLiteral l
  PList ps -> bracketed (commaSep (map (prettyPat PTop) ps))
  PTuple ps -> parenthesized (commaSep (map (prettyPat PTop) ps))
  PAs v q -> nameDoc (unVarName v) <> operator "@" <> prettyPat PArg q
  PCon c [] -> conDoc (unConName c)
  PCon c ps -> wrap PApply (hsep (conDoc (unConName c) : map (prettyPat PArg) ps))
  POpChain p0 rs ->
    wrap POp $
      hsep $
        prettyPat PApply p0
          : concat [[opDoc op, prettyPat PApply x] | (op, x) <- rs]
  where
    wrap = parenWhen ctx

parenWhen :: Prec -> Prec -> Doc Ann -> Doc Ann
parenWhen ctx lvl d = if ctx > lvl then parenthesized d else d

symDoc :: (Doc Ann -> Doc Ann) -> String -> Doc Ann
symDoc ann s
  | isOperatorName s = parenthesized (ann (pretty s))
  | otherwise = ann (pretty s)

nameDoc :: String -> Doc Ann
nameDoc = symDoc varName

conDoc :: String -> Doc Ann
conDoc = symDoc conName

opDoc :: String -> Doc Ann
opDoc s
  | isOperatorName s = operator (pretty s)
  | otherwise = punct "`" <> operator (pretty s) <> punct "`"

prettyLiteral :: Literal -> Doc Ann
prettyLiteral (LInt n) = literal (pretty n)
prettyLiteral (LChar c) = literal ("'" <> pretty (escape [c]) <> "'")
prettyLiteral (LString s) = literal ("\"" <> pretty (escape s) <> "\"")

escape :: String -> String
escape s = [c | ch <- s, c <- escapeChar ch]

escapeChar :: Char -> String
escapeChar c = case c of
  '\n' -> "\\n"
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\\' -> "\\\\"
  '\'' -> "\\'"
  '"' -> "\\\""
  _ -> [c]
