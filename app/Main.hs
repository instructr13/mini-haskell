{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Args
import Options.Applicative (execParser)
import TRS
import TRSParser

main :: IO ()
main = execParser argParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain Argument {..} = do
  result <- readTRSFile file

  case result of
    Left e -> error (show e)
    Right trs -> do
      putStr (showTRS trs) -- Output a normal form of F "main" [].
