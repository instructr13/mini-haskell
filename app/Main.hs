{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Control.Exception (SomeException, try)
import Options.Applicative (execParser)
import Prettyprinter
import Render
import Repl
import Source
import System.Exit (exitFailure)
import System.IO
import TRS
import TRS.Pretty

main :: IO ()
main = execParser argParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain args = do
  out <- mkOut (color args) stdout
  err <- mkOut (color args) stderr

  case file args of
    Just f -> runFile out err f
    Nothing -> do
      hSetBuffering stdout LineBuffering
      hPutDocLn out banner
      loop (ReplEnv out err) initState

-- 拡張子が .hs なら Haskell としてコンパイルし、それ以外は TRS として読む。
runFile :: Out -> Out -> FilePath -> IO ()
runFile out err f = do
  read' <- try (readFile f) :: IO (Either SomeException String)
  case read' of
    Left e ->
      die
        err
        ( errorDoc
            ( "cannot read"
                <+> filepath (pretty f)
                <> ":"
                <+> pretty (takeWhile (/= '\n') (show e))
            )
        )
    Right src -> case sourceTRS f src of
      Left e -> die err (sourceErrorDoc e)
      Right trs -> do
        hPutDocLn out (prettyTRS trs)
        case lookup (F "main" []) trs of
          Nothing ->
            die
              err
              ( errorDoc
                  ( "no rule for"
                      <+> funName "main"
                      <+> "in"
                      <+> filepath (pretty f)
                  )
              )
          Just mainF -> do
            hPutDocLn out mempty
            hPutDocLn
              out
              (faint "-->" <+> prettyTermWith (definedSymbols trs) (nf trs mainF))

die :: Out -> Doc Ann -> IO ()
die err d = hPutDocLn err d >> exitFailure
