{-# LANGUAGE OverloadedStrings #-}

module Source
  ( SourceKind (..),
    sourceKind,
    SourceError (..),
    sourceTRS,
    sourceTRSWith,
    mainTerm,
    firstLine,
    readSource,
    loadSource,
    loadMain,
    loadMainWith,
    loadSourceWith,
    sourceErrorDoc,
    trsErrorDoc,
    lexErrorDoc,
    compileErrorDoc,
    multilineErrorDoc,
    violationDoc,
    violationsDoc,
    haskellNotation,
  )
where

import Control.Exception (SomeException, try)
import Data.List (isSuffixOf)
import Data.Set (Set)
import HS.Check
import HS.Compile
import HS.Error (CompileError (..), featureName)
import HS.Lexer (LexError, prettyLexError)
import HS.Name
import Prettyprinter
import Render
import TRS
import TRS.Parser (TRSParseError, prettyParseError, readTRS)
import TRS.Pretty

data SourceKind = TrsSource | HaskellSource
  deriving (Eq, Show)

sourceKind :: FilePath -> SourceKind
sourceKind fp
  | ".hs" `isSuffixOf` fp = HaskellSource
  | otherwise = TrsSource

data SourceError
  = ReadError FilePath String
  | TrsError TRSParseError
  | HaskellError CompileError
  | NoMain FilePath

sourceTRS :: FilePath -> String -> Either SourceError TRS
sourceTRS = sourceTRSWith defaultOptions

sourceTRSWith :: Options -> FilePath -> String -> Either SourceError TRS
sourceTRSWith opts fp src = case sourceKind fp of
  TrsSource -> either (Left . TrsError) Right (readTRS src)
  HaskellSource -> either (Left . HaskellError) Right (compileWith opts fp src)

-- The spellings the compiler accepts for the wired-in constructors are the
-- ones the printer folds back into literal notation, so both come from
-- knownSpec and cannot drift apart.
haskellNotation :: Notation
haskellNotation =
  Notation
    { noZero = knownNames KnZero,
      noSucc = knownNames KnSucc,
      noNil = knownNames KnNil,
      noCons = knownNames KnCons
    }

mainTerm :: Term
mainTerm = F "main" []

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

readSource :: FilePath -> IO (Either SourceError String)
readSource fp = do
  r <- try (readFile fp)
  pure $ case r of
    Left e -> Left (ReadError fp (firstLine (show (e :: SomeException))))
    Right src -> Right src

loadSource :: FilePath -> IO (Either SourceError TRS)
loadSource = loadSourceWith defaultOptions

loadSourceWith :: Options -> FilePath -> IO (Either SourceError TRS)
loadSourceWith opts fp = do
  r <- readSource fp
  pure $ case r of
    Left e -> Left e
    Right src -> sourceTRSWith opts fp src

-- Read a file, turn it into a TRS, and find main's right-hand side. The one
-- path shared by the CLI and the REPL.
loadMain :: FilePath -> IO (Either SourceError (TRS, Term))
loadMain = loadMainWith defaultOptions

loadMainWith :: Options -> FilePath -> IO (Either SourceError (TRS, Term))
loadMainWith opts fp = do
  r <- loadSourceWith opts fp
  pure $ do
    trs <- r
    body <- maybe (Left (NoMain fp)) Right (lookup mainTerm trs)
    pure (trs, body)

sourceErrorDoc :: SourceError -> Doc Ann
sourceErrorDoc (ReadError fp msg) =
  errorDoc ("cannot read" <+> filepath (pretty fp) <> ":" <+> pretty msg)
sourceErrorDoc (NoMain fp) =
  errorDoc ("no rule for" <+> funName "main" <+> "in" <+> filepath (pretty fp))
sourceErrorDoc (TrsError e) = trsErrorDoc e
sourceErrorDoc (HaskellError e) = compileErrorDoc e

multilineErrorDoc :: Doc Ann -> String -> Doc Ann
multilineErrorDoc title msg =
  errorDoc title <> nest 2 (hardline <> vsep (map (faint . pretty) (lines msg)))

violationsDoc :: TRS -> [Violation] -> [Doc Ann]
violationsDoc _ [] = [annotate AOk "ok" <+> "(no violations)"]
violationsDoc trs vs =
  errorDoc (number (length vs) <+> "violation(s):")
    : map (indent 2 . violationDoc (definedSymbols trs)) vs

violationDoc :: Set String -> Violation -> Doc Ann
violationDoc defined v = case v of
  LhsIsVariable a -> withRule a "variable left-hand side"
  UnboundRhsVar a x ->
    withRule a ("unbound right-hand side variable" <+> varName (pretty x))
  NonLeftLinear a x ->
    withRule a ("non-linear pattern variable" <+> varName (pretty x))
  ArityMismatch a f want got ->
    withRule
      a
      ( funName (pretty f)
          <+> "used at arity"
          <+> number got
          <> ", expected"
          <+> number want
      )
  NotConstructorSystem a f ->
    withRule a ("defined symbol" <+> funName (pretty f) <+> "occurs in a pattern")
  RootOverlap a b ->
    errorDoc ("rule" <+> ruleTag a <+> "overlaps rule" <+> ruleTag b)
      <> nest 2 (hardline <> prettyRuleWith haskellNotation defined (atRule a))
      <> nest 2 (hardline <> prettyRuleWith haskellNotation defined (atRule b))
  SymbolClash f ->
    errorDoc (funName (pretty f) <+> "is both a constructor and a defined symbol")
  where
    ruleTag a = faint ("#" <> number (atId a))
    withRule a msg =
      errorDoc ("rule" <+> ruleTag a <> ":" <+> msg)
        <> nest 2 (hardline <> prettyRuleWith haskellNotation defined (atRule a))

trsErrorDoc :: TRSParseError -> Doc Ann
trsErrorDoc = multilineErrorDoc "parse error" . prettyParseError

lexErrorDoc :: LexError -> Doc Ann
lexErrorDoc = multilineErrorDoc "lex error" . prettyLexError

compileErrorDoc :: CompileError -> Doc Ann
compileErrorDoc e = case e of
  SyntaxError _ msg -> multilineErrorDoc "syntax error" msg
  UnknownConstructor c -> errorDoc ("unknown constructor" <+> conName (pretty (show c)))
  UnknownVariable v -> errorDoc ("unknown variable" <+> varName (pretty (show v)))
  ArityError f want got ->
    errorDoc
      ( funName (pretty f)
          <+> "expects"
          <+> number want
          <+> "argument(s) but got"
          <+> number got
      )
  ClauseArityMismatch v a b ->
    errorDoc
      ( "clauses of"
          <+> funName (pretty (show v))
          <+> "have different arities:"
          <+> number a
          <+> "and"
          <+> number b
      )
  OverlappingClauses v -> errorDoc ("overlapping clauses in" <+> funName (pretty (show v)))
  DuplicateDefinition v -> errorDoc ("duplicate definition of" <+> funName (pretty (show v)))
  FixityConflict a b ->
    errorDoc ("conflicting fixities:" <+> pretty a <+> "and" <+> pretty b)
  FixityLevelOutOfRange op n ->
    errorDoc ("fixity level" <+> number n <+> "for" <+> operator (pretty op) <+> "is not 0..9")
  MissingKnownName k name arity ->
    errorDoc ("this module needs" <+> conName (pretty name) <+> "of arity" <+> number arity)
      <> hardline
      <> faint ("note: required as the wired-in name " <> pretty (knownName k))
  KnownNameMismatch k name want got ->
    errorDoc
      ( conName (pretty name)
          <+> "must be a constructor of arity"
          <+> number want
          <> ", but has arity"
          <+> number got
      )
      <> hardline
      <> faint ("note: required as the wired-in name " <> pretty (knownName k))
  NoLiteralRepresentation rep lit ->
    errorDoc
      ( "the literal"
          <+> literal (pretty lit)
          <+> "has no representation under --numeric"
          <+> pretty (numericName rep)
      )
  SymbolCollision name -> errorDoc ("the name" <+> funName (pretty name) <+> "is defined twice")
  Unsupported f -> errorDoc ("not supported yet:" <+> pretty (featureName f))
  InternalError msg ->
    errorDoc ("internal error:" <+> pretty msg)
      <> hardline
      <> faint "note: this is a compiler bug, not a problem with the source"
