module ApplicativeTRS.Syntax (module ApplicativeTRS.Syntax) where

import ApplicativeTRS.Lexer
import ApplicativeTRS.TokenStream
import Data.Char (isUpper)
import qualified Data.Set as Set
import Data.Void
import Text.Megaparsec hiding (Token)

type Parser = Parsec Void TokenStream

data SExpr
  = SEIdent String
  | SEApp SExpr SExpr
  deriving (Show)

-- Split an application spine into its head symbol and its arguments.
-- spine (SEIdent "map") = ("map", [])
-- spine (SEApp (SEApp (SEIdent "map") f) xs) = ("map", [f, xs])
spine :: SExpr -> (String, [SExpr])
spine = go []
  where
    go acc (SEApp f x) = go (x : acc) f
    go acc (SEIdent x) = (x, acc)

-- root(t)
spineHead :: SExpr -> String
spineHead = fst . spine

-- If the symbol starts with an upper case character, Haskell treats it as a constructor.
-- Every other identifier is a variable, unless it is a defined symbol.
isConName :: String -> Bool
isConName (c : _) = isUpper c
isConName [] = False

data ConDecl = ConDecl {cdName :: String, cdArity :: Int}

data DataDecl = DataDecl {ddName :: String, ddCons :: [ConDecl]}

data AppModule = AppModule {amData :: [DataDecl], amRules :: [AppRule]}

instance Semigroup AppModule where
  AppModule d1 r1 <> AppModule d2 r2 = AppModule (d1 <> d2) (r1 <> r2)

instance Monoid AppModule where
  mempty = AppModule [] []

type AppRule = (SExpr, SExpr)

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

-- An identifier that names a constructor or a type constructor.
conIdent :: Parser String
conIdent = try (satisfyT check) <?> "constructor name"
  where
    check :: Token -> Maybe String
    check (TIdent s') | isConName s' = Just s'
    check _ = Nothing

-- An identifier that names a type variable.
varIdent :: Parser String
varIdent = try (satisfyT check) <?> "type variable"
  where
    check :: Token -> Maybe String
    check (TIdent s') | not (isConName s') = Just s'
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
