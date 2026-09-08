{-# LANGUAGE OverloadedStrings #-}

module Source (module Source) where

import Data.List (isSuffixOf)
import Prettyprinter
import TRS
import TRS.Error (TRSError (..))
import TRS.Loader
import Util (mapLeft)

data SourceKind = TRSSource | HSSource deriving (Eq, Show)

data SourceError
  = SrcTRSError TRSError
  | SrcHSError
  | UnknownSrcError

sourceKind :: FilePath -> Either SourceError SourceKind
sourceKind file
  | ".trs" `isSuffixOf` file = Right TRSSource
  | ".hs" `isSuffixOf` file = Right HSSource
  | otherwise = Left UnknownSrcError

errorDoc :: Doc ann -> Doc ann
errorDoc d = "error:" <+> d

multilineErrorDoc :: Doc ann -> String -> Doc ann
multilineErrorDoc title msg =
  errorDoc title <> nest 2 (hardline <> vsep (map pretty (lines msg)))

sourceErrorDoc :: SourceError -> Doc ann
sourceErrorDoc (SrcTRSError e) = trsErrorDoc e
sourceErrorDoc SrcHSError = undefined
sourceErrorDoc UnknownSrcError = "unknown source file (only .trs and .hs are supported)"

trsErrorDoc :: TRSError -> Doc ann
trsErrorDoc (SyntaxError _ msg) =
  multilineErrorDoc "syntax error" msg <> hardline
trsErrorDoc e = errorDoc (pretty (show e))

compileSrc :: FilePath -> String -> Either SourceError TRS
compileSrc file src = case sourceKind file of
  Left e -> Left e
  Right TRSSource -> mapLeft SrcTRSError (loadTRS file src)
  Right HSSource -> undefined
