{-# LANGUAGE LambdaCase #-}

module HS.Lexer
  ( Token (..),
    PosToken (..),
    SrcPos (..),
    LexError,
    Parser,
    lexHS,
    prettyLexError,
    tokenWidth,
    isVirtual,
    unvirtual,
    showToken,
    keywords,
    reservedOps,
    specialChars,
  )
where

import Control.Monad (void)
import Data.Char
  ( chr,
    isAlpha,
    isAlphaNum,
    isSpace,
    isUpper,
  )
import Data.Void (Void)
import HS.Error (SrcPos (..))
import HS.Name (symbolChars)
import Text.Megaparsec hiding (Token, Tokens)
import Text.Megaparsec.Char (char, digitChar, string)

type Parser = Parsec Void String

type LexError = ParseErrorBundle String Void

-- | 位置とソース断片つきの読みやすいエラー表示。
prettyLexError :: LexError -> String
prettyLexError = errorBundlePretty

data Token
  = -- | @x@, @f@, @append@
    TVarId String
  | -- | @Nil@, @Cons@, @Bool@
    TConId String
  | -- | @++@, @&&@, @.@ (@:@ ==> Cons)
    TVarSym String
  | -- | @42@
    TInt Integer
  | -- | @'a'@
    TChar Char
  | -- | @"ab"@
    TString String
  | -- | @data@, @case@, @of@, @let@, @in@, @where@, …
    TKeyword String
  | -- | @=@ @->@ @|@ @\\@ @\@@ @_@ @::@
    TReservedOp String
  | -- | @( ) , ; [ ] ` { }@
    TSpecial Char
  | -- | @{@ by layout
    TVLBrace
  | -- | @;@ by layout
    TVSemi
  | -- | @}@ by layout
    TVRBrace
  deriving (Eq, Ord, Show)

data PosToken = PosToken
  { ptPos :: !SrcPos,
    ptTok :: !Token
  }
  deriving (Eq, Ord, Show)

isVirtual :: Token -> Bool
isVirtual t = t `elem` [TVLBrace, TVSemi, TVRBrace]

unvirtual :: Token -> Token
unvirtual TVLBrace = TSpecial '{'
unvirtual TVSemi = TSpecial ';'
unvirtual TVRBrace = TSpecial '}'
unvirtual t = t

showToken :: Token -> String
showToken = \case
  TVarId s -> s
  TConId s -> s
  TVarSym s -> s
  TInt n -> show n
  TChar c -> show c
  TString s -> show s
  TKeyword s -> s
  TReservedOp s -> s
  TSpecial c -> [c]
  TVLBrace -> "{ (layout)"
  TVSemi -> "; (layout)"
  TVRBrace -> "} (layout)"

keywords :: [String]
keywords =
  [ "case",
    "class",
    "data",
    "default",
    "deriving",
    "do",
    "else",
    "foreign",
    "if",
    "import",
    "in",
    "infix",
    "infixl",
    "infixr",
    "instance",
    "let",
    "module",
    "newtype",
    "of",
    "then",
    "type",
    "where"
  ]

reservedOps :: [String]
reservedOps = ["..", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"]

specialChars :: String
specialChars = "(),;[]`{}"

lexHS :: FilePath -> String -> Either LexError [PosToken]
lexHS = parse (whiteSpace *> many posToken <* eof)

posToken :: Parser PosToken
posToken = do
  p <- currentPos
  t <- pToken
  whiteSpace
  pure (PosToken p t)

currentPos :: Parser SrcPos
currentPos = do
  p <- getSourcePos
  pure (SrcPos (unPos (sourceLine p)) (unPos (sourceColumn p)))

pToken :: Parser Token
pToken =
  choice
    [ TString <$> stringLit,
      TChar <$> charLit,
      TInt <$> natural,
      identifierOrKeyword,
      symbolOrReservedOp,
      TSpecial <$> oneOf specialChars
    ]
    <?> "token"

identifierOrKeyword :: Parser Token
identifierOrKeyword = do
  c <- satisfy (\ch -> isAlpha ch || ch == '_')
  cs <- many (satisfy (\ch -> isAlphaNum ch || ch == '_' || ch == '\''))
  let s = c : cs
  pure $
    if s == "_"
      then TReservedOp "_"
      else
        if s `elem` keywords
          then TKeyword s
          else
            if isUpper c
              then TConId s
              else TVarId s

symbolOrReservedOp :: Parser Token
symbolOrReservedOp = do
  s <- some (oneOf symbolChars)
  pure $ if s `elem` reservedOps then TReservedOp s else TVarSym s

natural :: Parser Integer
natural = read <$> some digitChar

whiteSpace :: Parser ()
whiteSpace = hidden (skipMany (void (satisfy isSpace) <|> lineComment <|> blockComment))

lineComment :: Parser ()
lineComment = try $ do
  _ <- string "--"
  _ <- many (char '-')
  notFollowedBy (oneOf symbolChars)
  _ <- manyTill anySingle (void (char '\n') <|> eof)
  pure ()

-- | @{- … -}@
blockComment :: Parser ()
blockComment = void (string "{-") *> inBlock
  where
    inBlock =
      choice
        [ void (string "-}"),
          blockComment *> inBlock,
          anySingle *> inBlock
        ]

charLit :: Parser Char
charLit = between (char '\'') (char '\'') (escaped <|> noneOf "'\\")

stringLit :: Parser String
stringLit = between (char '"') (char '"') (many (escaped <|> noneOf "\"\\"))

escaped :: Parser Char
escaped =
  char '\\'
    *> choice
      [ '\n' <$ char 'n',
        '\t' <$ char 't',
        '\r' <$ char 'r',
        '\\' <$ char '\\',
        '\'' <$ char '\'',
        '"' <$ char '"',
        chr . read <$> some digitChar -- \65
      ]

tokenWidth :: Token -> Int
tokenWidth t
  | isVirtual t = 0
  | otherwise = length (showToken t)
