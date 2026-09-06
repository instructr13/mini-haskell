module Main (main) where

import Args
import Options.Applicative (execParser)
import TRS
import TRSParser

main :: IO ()
main = execParser argParserInfo >>= runMain

runMain :: Argument -> IO ()
runMain args = do
  result <- readTRSFile (file args)

  case result of
    Left e -> error (show e)
    Right trs -> do
      putStr (showTRS trs)

      -- Output a normal form of F "main" [].
      let maybeMain = lookup (F "main" []) trs

      case maybeMain of
        Nothing -> error "No main function found or is invalid"
        Just mainF -> do
          putStr "--> "
          putStrLn (show (nf trs mainF))
