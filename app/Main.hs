{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Args
import Control.Exception
import qualified Data.Set as Set
import HS.Pretty (prettyTerm)
import Options.Applicative (execParser)
import Prettyprinter
import Render (hPutDocLn)
import Source
import System.Exit (exitFailure)
import System.IO
import TRS.Pretty (prettyTRS)
import TRS.Rewrite (accessibleFunctions, indexTRS, nfIndexed, pruneIndexedTRS)
import Term

main :: IO ()
main = execParser argParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain args = do
  let srcFile = file args
  read' <- try (readFile srcFile) :: IO (Either SomeException String)

  case read' of
    Left e ->
      die
        ( errorDoc
            ("cannot read" <+> pretty srcFile <> ":" <+> pretty (takeWhile (/= '\n') (show e)))
        )
    Right src -> case compileSrc srcFile src of
      Left e -> die (sourceErrorDoc e)
      Right trs -> do
        let itrs = indexTRS trs
        let accessibles = accessibleFunctions itrs (F "main" [])

        hPutDocLn stdout (prettyTRS [(l, r) | (l@(F f _), r) <- trs, f `Set.member` accessibles])

        case lookup (F "main" []) trs of
          Nothing ->
            die
              (errorDoc ("no rule for main in " <+> pretty srcFile))
          Just mainF -> case (nfIndexed 10000 (pruneIndexedTRS itrs mainF) mainF) of
            (True, t) -> do
              hPutDocLn stdout mempty
              hPutDocLn stdout ("-->" <+> prettyTerm t)
            (False, t) ->
              die
                (errorDoc "maximum calculation limit exceeded (last term):" <+> prettyTerm t)

die :: Doc ann -> IO ()
die d = hPutDocLn stderr d >> exitFailure
