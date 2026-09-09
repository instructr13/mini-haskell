module ApplicativeTRS.Syntax (module ApplicativeTRS.Syntax) where

import ApplicativeTRS.Lexer
import ApplicativeTRS.TokenStream
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec hiding (Token)

type Parser = Parsec Void TokenStream

data SExpr
  = SEIdent String
  | SEApp SExpr SExpr
  deriving (Show)

type AppRule = (SExpr, SExpr)

data AppSectionSet = AppSectionSet {ssVars :: [String], ssRules :: [AppRule]}

satisfyT :: (Token -> Maybe a) -> Parser a
satisfyT f = token (f . unvirtual . ptTok) Set.empty

tok :: Token -> Parser ()
tok t = satisfyT check <?> showToken t
  where
    check :: Token -> Maybe ()
    check t' = if t' == unvirtual t then Just () else Nothing

ident :: Parser String
ident = satisfyT check <?> "identifier"
  where
    check :: Token -> Maybe String
    check (TIdent s') = Just s'
    check _ = Nothing

keyword :: String -> Parser ()
keyword s = tok (TKeyword s)

op :: String -> Parser ()
op s = tok (TOp s)

special :: Char -> Parser ()
special c = tok (TSpecial c)

semi :: Parser ()
semi = special ';'

itemsOf :: Parser a -> Parser [a]
itemsOf p = skipMany semi *> many (p <* skipMany semi)

parens :: Parser a -> Parser a
parens = between (special '(') (special ')')

squareBrackets :: Parser a -> Parser a
squareBrackets = between (special '[') (special ']')

-- SEApp (SEApp (f a)) b ==> (f, [a,b])
sSpine :: SExpr -> (String, [SExpr])
sSpine = go []
  where
    go acc (SEApp f x) = go (x : acc) f
    go acc (SEIdent n) = (n, acc)
