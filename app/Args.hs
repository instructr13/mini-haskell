module Args (Argument (..), argParserInfo) where

import Data.Version (showVersion)
import Options.Applicative
import Paths_mini_haskell (version)
import Render (ColorMode (..))

data Argument = Argument
  { file :: Maybe String,
    color :: ColorMode
  }
  deriving (Show)

argParser :: Parser Argument
argParser =
  Argument
    <$> optional
      ( strArgument
          ( metavar "FILE"
              <> help "TRS (.trs) or Haskell (.hs) file to evaluate; omit to open the REPL"
          )
      )
    <*> colorOption

colorOption :: Parser ColorMode
colorOption =
  option
    (eitherReader readColorMode)
    ( long "color"
        <> metavar "WHEN"
        <> value ColorAuto
        <> showDefaultWith showColorMode
        <> help "Colorize output: auto, always or never"
    )

readColorMode :: String -> Either String ColorMode
readColorMode s = case s of
  "auto" -> Right ColorAuto
  "always" -> Right ColorAlways
  "never" -> Right ColorNever
  _ -> Left ("expected auto, always or never, but got " ++ show s)

showColorMode :: ColorMode -> String
showColorMode ColorAuto = "auto"
showColorMode ColorAlways = "always"
showColorMode ColorNever = "never"

versionOption :: Parser (a -> a)
versionOption =
  infoOption
    ("mini-haskell " ++ showVersion version)
    (long "version" <> short 'V' <> help "Show the version and exit")

argParserInfo :: ParserInfo Argument
argParserInfo =
  info
    (argParser <**> helper <**> versionOption)
    ( fullDesc
        <> header "mini-haskell - a term rewriting system playground"
        <> progDesc
          "Evaluate FILE by rewriting the term main() to its normal form. \
          \A .hs file is compiled to a TRS first. \
          \Without FILE, open an interactive REPL."
        <> footer
          "Examples: mini-haskell test/trs/add.trs | mini-haskell (REPL)"
    )
