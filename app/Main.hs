{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Data.Set (Set)
import Options.Applicative (execParser)
import Prettyprinter
import Render
import HS.Compile
import HS.Monad (allLayers, coreOnly)
import Repl
import Source
import System.Exit (exitFailure)
import System.IO
import TRS
import TRS.Pretty

main :: IO ()
main = do
  args <- execParser argParserInfo
  runMain args

runMain :: Argument -> IO ()
runMain args = do
  out <- mkOut (color args) stdout
  err <- mkOut (color args) stderr

  let opts =
        defaultOptions
          { optNumeric = numericMode args,
            optLayers = if coreOnlyMode args then coreOnly else allLayers
          }
      st = initState {rsOptions = opts}

  case file args of
    Just f -> runFile out err opts (rsLimit st) f
    Nothing -> do
      hSetBuffering stdout LineBuffering
      hPutDocLn out banner
      loop (ReplEnv out err) st

runFile :: Out -> Out -> Options -> Int -> FilePath -> IO ()
runFile out err opts limit f =
  do
  r <- loadMainWith opts f
  case r of
    Left e -> die err (sourceErrorDoc e)
    Right (trs, body) -> do
      hPutDocLn out (prettyTRSNotation haskellNotation trs)
      hPutDocLn out mempty
      hPutDocLn out (resultDoc (definedSymbols trs) (nfWithLimit limit trs body))

resultDoc :: Set String -> Either Term Term -> Doc Ann
resultDoc defined r = case r of
  Right u -> faint "-->" <+> term u
  Left u -> warnDoc "limit reached; stopped at:" <+> term u
  where
    term = prettyTermWith haskellNotation defined

die :: Out -> Doc Ann -> IO ()
die err d = do
  hPutDocLn err d
  exitFailure
