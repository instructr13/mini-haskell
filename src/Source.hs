{-# LANGUAGE OverloadedStrings #-}

module Source
  ( SourceKind (..),
    sourceKind,
    SourceError (..),
    sourceTRS,
    sourceErrorDoc,
    trsErrorDoc,
    lexErrorDoc,
    compileErrorDoc,
    multilineErrorDoc,
  )
where

import Data.List (isSuffixOf)
import HS.Compile (compileModule)
import HS.Error (CompileError (..))
import HS.Lexer (LexError, prettyLexError)
import Prettyprinter
import Render
import TRS (TRS)
import TRS.Parser (TRSParseError, prettyParseError, readTRS)

data SourceKind = TrsSource | HaskellSource
  deriving (Eq, Show)

sourceKind :: FilePath -> SourceKind
sourceKind fp
  | ".hs" `isSuffixOf` fp = HaskellSource
  | otherwise = TrsSource

data SourceError
  = TrsError TRSParseError
  | HaskellError CompileError

sourceTRS :: FilePath -> String -> Either SourceError TRS
sourceTRS fp src = case sourceKind fp of
  TrsSource -> either (Left . TrsError) Right (readTRS src)
  HaskellSource -> either (Left . HaskellError) Right (compileModule fp src)

sourceErrorDoc :: SourceError -> Doc Ann
sourceErrorDoc (TrsError e) = trsErrorDoc e
sourceErrorDoc (HaskellError e) = compileErrorDoc e

multilineErrorDoc :: Doc Ann -> String -> Doc Ann
multilineErrorDoc title msg =
  errorDoc title <> nest 2 (hardline <> vsep (map (faint . pretty) (lines msg)))

trsErrorDoc :: TRSParseError -> Doc Ann
trsErrorDoc = multilineErrorDoc "parse error" . prettyParseError

lexErrorDoc :: LexError -> Doc Ann
lexErrorDoc = multilineErrorDoc "lex error" . prettyLexError

compileErrorDoc :: CompileError -> Doc Ann
compileErrorDoc (SyntaxError _ msg) =
  multilineErrorDoc "syntax error" msg
    <> hardline
    <> faint "note: layout is not implemented yet; separate declarations with ';' or '{ ... }'"
compileErrorDoc e = errorDoc (pretty (show e))
