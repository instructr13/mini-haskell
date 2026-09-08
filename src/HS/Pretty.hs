{-# LANGUAGE OverloadedStrings #-}

module HS.Pretty
  ( renderCompact,
    renderWide,
    pprintModule,
    pprintDecl,
    prettyModule,
    prettyDecl,
    prettyExpr,
    prettyPat,
    prettyLiteral,
    Prec (..),
  )
where

-- AI-Generated Pretty Printer

import HS.Name
import HS.Syntax
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Render

-- | 折り返さずに一行で描画する。
renderCompact :: Doc Ann -> String
renderCompact = renderString . layoutPretty (LayoutOptions Unbounded) . unAnnotate

renderWide :: Doc Ann -> String
renderWide = renderPlain 100

pprintModule :: Module -> String
pprintModule = renderCompact . prettyModule

pprintDecl :: Decl -> String
pprintDecl = renderCompact . prettyDecl

prettyModule :: Module -> Doc Ann
prettyModule ds =
  concatWith (\a b -> a <> space <> punct ";" <> hardline <> b) (concatMap declLines ds)

declLines :: Decl -> [Doc Ann]
declLines (DFun n cs) = map (prettyClause n) cs
declLines d@(DData _ _ _) = [prettyDecl d]
declLines d@(DFixity _ _ _) = [prettyDecl d]

prettyDecl :: Decl -> Doc Ann
prettyDecl (DData t vs cs) =
  hsep $
    [keyword "data", conName (pretty (unTyConName t))]
      ++ map (varName . pretty . unVarName) vs
      ++ [operator "=", concatWith (\a b -> a <+> punct "|" <+> b) (map conDecl cs)]
  where
    conDecl (c, n) = hsep (conName (pretty (unConName c)) : replicate n (varName "a"))
prettyDecl (DFixity a n ops) =
  hsep [keyword (assoc a), number n, concatWith (\x y -> x <> punct "," <+> y) (map opDoc ops)]
  where
    assoc AssocLeft = "infixl"
    assoc AssocRight = "infixr"
    assoc AssocNone = "infix"
prettyDecl (DFun n cs) = vsep (map (prettyClause n) cs)

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
prettyWhere ds = space <> keyword "where" <+> braceBlock (concatMap declLines ds)

braceBlock :: [Doc Ann] -> Doc Ann
braceBlock ds =
  punct "{" <+> concatWith (\a b -> a <> punct ";" <+> b) ds <+> punct "}"

data Prec = PTop | POp | PApply | PArg deriving (Eq, Ord, Show)

prettyExpr :: Prec -> Expr -> Doc Ann
prettyExpr ctx e = case e of
  EVar v -> nameDoc (unVarName v)
  ECon c -> conDoc (unConName c)
  ELiteral l -> prettyLiteral l
  EList es -> brackets' (commaSep (map (prettyExpr PTop) es))
  ETuple es -> parens' (commaSep (map (prettyExpr PTop) es))
  ESectionL op x -> parens' (prettyExpr PApply x <+> opDoc op)
  ESectionR op x -> parens' (opDoc op <+> prettyExpr PApply x)
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
          <+> braceBlock (concatMap declLines ds)
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
    wrap lvl d = if ctx > lvl then parens' d else d

prettyAlt :: Alt -> Doc Ann
prettyAlt (Alt p rhs ws) =
  prettyPat PTop p <> prettyRhs (operator "->") rhs <> prettyWhere ws

prettyPat :: Prec -> Pat -> Doc Ann
prettyPat ctx p = case p of
  PVar v -> nameDoc (unVarName v)
  PWild -> varName "_"
  PLiteral l -> prettyLiteral l
  PList ps -> brackets' (commaSep (map (prettyPat PTop) ps))
  PTuple ps -> parens' (commaSep (map (prettyPat PTop) ps))
  PAs v q -> nameDoc (unVarName v) <> operator "@" <> prettyPat PArg q
  PCon c [] -> conDoc (unConName c)
  PCon c ps -> wrap PApply (hsep (conDoc (unConName c) : map (prettyPat PArg) ps))
  where
    wrap lvl d = if ctx > lvl then parens' d else d

nameDoc :: String -> Doc Ann
nameDoc s
  | isOperatorName s = parens' (operator (pretty s))
  | otherwise = varName (pretty s)

conDoc :: String -> Doc Ann
conDoc s
  | isOperatorName s = parens' (conName (pretty s))
  | otherwise = conName (pretty s)

opDoc :: String -> Doc Ann
opDoc s
  | isOperatorName s = operator (pretty s)
  | otherwise = punct "`" <> operator (pretty s) <> punct "`"

prettyLiteral :: Literal -> Doc Ann
prettyLiteral (LInt n) = literal (pretty n)
prettyLiteral (LChar c) = literal ("'" <> pretty (escape [c]) <> "'")
prettyLiteral (LString s) = literal ("\"" <> pretty (escape s) <> "\"")

escape :: String -> String
escape = concatMap $ \c -> case c of
  '\n' -> "\\n"
  '\t' -> "\\t"
  '\r' -> "\\r"
  '\\' -> "\\\\"
  '\'' -> "\\'"
  '"' -> "\\\""
  _ -> [c]

commaSep :: [Doc Ann] -> Doc Ann
commaSep = hsep . punctuate (punct ",")

parens' :: Doc Ann -> Doc Ann
parens' d = punct "(" <> d <> punct ")"

brackets' :: Doc Ann -> Doc Ann
brackets' d = punct "[" <> d <> punct "]"
