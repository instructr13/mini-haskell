module TRS.Parser
  ( Parser,
    TRSParseError,
    convert,
    readTRSFile,
    readTRS,
    readTerm,
    prettyParseError,
    parseTerm,
    parseRule,
    parseTRS,
  )
where

import Data.List (nub)
import Data.Void (Void)
import TRS (Rule, Subst, TRS, Term (..), substitute, variables)
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C

type Parser = Parsec Void String

space :: Parser ()
space = hidden C.space

type TRSParseError = ParseErrorBundle String Void

prettyParseError :: TRSParseError -> String
prettyParseError = errorBundlePretty

variablesInTRS :: TRS -> [String]
variablesInTRS trs =
  nub [x | (l, r) <- trs, t <- [l, r], x <- variables t]

substituteTRS :: TRS -> Subst -> TRS
substituteTRS trs sigma =
  [ (substitute l sigma, substitute r sigma)
  | (l, r) <- trs
  ]

convert :: [String] -> Term -> Term
convert xs t = substitute t sigma
  where
    sigma = [(x, F x []) | x <- variables t, not (x `elem` xs)]

convertTRS :: [String] -> TRS -> TRS
convertTRS xs trs = substituteTRS trs sigma
  where
    sigma = [(x, F x []) | x <- variablesInTRS trs, not (x `elem` xs)]

-- Scanners.

identifier :: Parser String
identifier = label "identifier" $ do
  space
  x <- some (noneOf "(), \t\r\n")
  space
  return x

keyword :: String -> Parser ()
keyword s = label (show s) $ do
  space
  _ <- C.string s
  space
  return ()

-- Parsing functions.

parseTerm :: Parser Term
parseTerm = do
  f <- identifier
  args <- optional (keyword "(" *> sepBy parseTerm (keyword ",") <* keyword ")")

  return (maybe (V f) (F f) args)

parseRule :: Parser Rule
parseRule = do
  l <- parseTerm
  keyword "->"
  r <- parseTerm
  return (l, r)

parseVAR :: Parser ([String], TRS)
parseVAR = do
  keyword "VAR"
  xs <- many identifier
  return (xs, [])

parseRULES :: Parser ([String], TRS)
parseRULES = do
  keyword "RULES"
  rs <- many parseRule
  return ([], rs)

parseAnything :: Parser ()
parseAnything =
  do _ <- identifier; return ()
    <|> do keyword "("; _ <- many parseAnything; keyword ")"

parseComment :: Parser ([String], TRS)
parseComment = do
  _ <- many parseAnything
  return ([], [])

parseSection :: Parser ([String], TRS)
parseSection = do
  keyword "("
  (xs, trs) <- try parseVAR <|> try parseRULES <|> parseComment
  keyword ")"
  return (xs, trs)

parseTRS :: Parser TRS
parseTRS = do
  ps <- many parseSection
  space
  eof
  let (xss, trss) = unzip ps
   in return (convertTRS (concat xss) (concat trss))

readTRS :: String -> Either TRSParseError TRS
readTRS = parse parseTRS "<string>"

readTRSFile :: FilePath -> IO (Either TRSParseError TRS)
readTRSFile path = parse parseTRS path <$> readFile path

readTerm :: [String] -> String -> Either TRSParseError Term
readTerm vs = fmap (convert vs) . parse (space *> parseTerm <* eof) "<input>"
