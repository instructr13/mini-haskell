{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Control.Exception
import Options.Applicative (execParser)
import Prettyprinter
import Render (hPutDocLn)
import Source
import System.Exit (exitFailure)
import System.IO
import TRS
import Term

main :: IO ()
main = execParser argParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain args = do
  let f = file args
  read' <- try (readFile f) :: IO (Either SomeException String)

  case read' of
    Left e ->
      die
        ( errorDoc
            ("cannot read" <+> pretty f <> ":" <+> pretty (takeWhile (/= '\n') (show e)))
        )
    Right src -> case compileSrc f src of
      Left e -> die (sourceErrorDoc e)
      Right trs -> do
        hPutDocLn stdout (prettyTRS trs)

        case lookup (F "main" []) trs of
          Nothing ->
            die
              (errorDoc ("no rule for main in " <+> pretty f))
          Just mainF -> do
            hPutDocLn stdout mempty
            hPutDocLn stdout ("-->" <+> pretty (nf trs mainF))

die :: Doc ann -> IO ()
die d = hPutDocLn stderr d >> exitFailure
