module Args (Argument (..), argParserInfo) where

import Options.Applicative

withInfo :: Parser a -> String -> ParserInfo a
withInfo p = info (p <**> helper) . progDesc

data Argument = Argument
  { file :: String
  }
  deriving (Read, Show)

argParser :: Parser Argument
argParser =
  Argument
    <$> strArgument (metavar "<FILE>" <> help "Input file")

argParserInfo :: ParserInfo Argument
argParserInfo = argParser `withInfo` "compute a TRS file"
