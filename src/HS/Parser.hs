{-# LANGUAGE LambdaCase #-}

module HS.Parser
  ( parseHS,
    parseTokens,
    groupClauses,
  )
where

import Control.Monad (void)
import Data.List (nub)
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Void (Void)
import HS.Error (CompileError (..))
import HS.Lexer
import HS.Name
import HS.Syntax
import HS.TokenStream
import Text.Megaparsec hiding (Token, Tokens)

type P = Parsec Void TokenStream

parseHS ::
  ([PosToken] -> [PosToken]) ->
  FilePath ->
  String ->
  Either CompileError Module
parseHS lay name src = do
  toks <- mapLeft syntaxError (lexHS name src)
  parseTokens name src (lay toks)

parseTokens :: FilePath -> String -> [PosToken] -> Either CompileError Module
parseTokens name src toks = do
  ds <- mapLeft syntaxError (parse (pModule <* eof) name (tokenStream src toks))
  groupClauses ds

syntaxError :: (TraversableStream s, VisualStream s) => ParseErrorBundle s Void -> CompileError
syntaxError e = SyntaxError (bundleErrorPos e) (errorBundlePretty e)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f = either (Left . f) Right

satisfyT :: (Token -> Maybe a) -> P a
satisfyT f = token (f . unvirtual . ptTok) Set.empty

tok :: Token -> P ()
tok t =
  void (satisfyT (\t' -> if t' == unvirtual t then Just () else Nothing))
    <?> showToken t

keyword :: String -> P ()
keyword s = tok (TKeyword s)

reservedOp :: String -> P ()
reservedOp s = tok (TReservedOp s)

special :: Char -> P ()
special c = tok (TSpecial c)

varName :: P VarName
varName = satisfyT (\case TVarId s -> Just (VarName s); _ -> Nothing) <?> "variable name"

conName :: P ConName
conName = satisfyT (\case TConId s -> Just (ConName s); _ -> Nothing) <?> "con name"

tyConName :: P TyConName
tyConName = satisfyT (\case TConId s -> Just (TyConName s); _ -> Nothing) <?> "tyCon name"

varSym :: P String
varSym = satisfyT (\case TVarSym s -> Just s; _ -> Nothing) <?> "operator"

intLiteral :: P Integer
intLiteral = satisfyT (\case TInt n -> Just n; _ -> Nothing) <?> "integer"

pLiteral :: P Literal
pLiteral =
  satisfyT
    ( \case
        TInt n -> Just (LInt n)
        TChar c -> Just (LChar c)
        TString s -> Just (LString s)
        _ -> Nothing
    )
    <?> "literal"

pOpName :: P String
pOpName = varSym <|> backquoted
  where
    backquoted =
      between
        (special '`')
        (special '`')
        ((unVarName <$> varName) <|> (unConName <$> conName))

parens :: P a -> P a
parens = between (special '(') (special ')')

brackets :: P a -> P a
brackets = between (special '[') (special ']')

semi :: P ()
semi = special ';'

-- | @{ d ; d ; d }@
blockOf :: P a -> P [a]
blockOf p = between (special '{') (special '}') (itemsOf p)

itemsOf :: P a -> P [a]
itemsOf p = skipMany semi *> sepEndBy p (skipSome semi)

pModule :: P [Decl]
pModule = catMaybes <$> (blockOf pDecl <|> itemsOf pDecl)

pDecl :: P (Maybe Decl)
pDecl =
  choice
    [ Nothing <$ try pTypeSig,
      Just <$> pData,
      Just <$> pFixity,
      Just <$> pClauseDecl
    ]

-- | @f, g :: T@
pTypeSig :: P ()
pTypeSig = do
  _ <- sepBy1 pFunName (special ',')
  reservedOp "::"
  skipToEndOfDecl

skipToEndOfDecl :: P ()
skipToEndOfDecl = skipSome (satisfyT notEnd)
  where
    notEnd t
      | t `elem` [TSpecial ';', TSpecial '}'] = Nothing
      | otherwise = Just ()

pData :: P Decl
pData = do
  keyword "data"
  t <- tyConName
  vs <- many varName
  reservedOp "="
  cs <- sepBy1 pConDecl (reservedOp "|")
  _ <- optional pDeriving
  pure (DData t vs cs)

pConDecl :: P (ConName, Int)
pConDecl = do
  c <- conName
  as <- many pAType
  pure (c, length as)

pAType :: P ()
pAType =
  choice
    [ void varName,
      void conName,
      void (parens (skipMany pTypeItem)),
      void (brackets (skipMany pTypeItem))
    ]

pTypeItem :: P ()
pTypeItem =
  choice
    [ pAType,
      void
        ( satisfyT
            ( \case
                TVarSym s -> Just s
                TReservedOp "->" -> Just "->"
                TSpecial ',' -> Just ","
                _ -> Nothing
            )
        )
    ]

pDeriving :: P ()
pDeriving = keyword "deriving" *> (void conName <|> void (parens (sepBy conName (special ','))))

pFixity :: P Decl
pFixity = do
  a <-
    choice
      [ AssocLeft <$ keyword "infixl",
        AssocRight <$ keyword "infixr",
        AssocNone <$ keyword "infix"
      ]
  n <- option 9 (fromInteger <$> intLiteral)
  ops <- sepBy1 pOpName (special ',')
  pure (DFixity a n ops)

pClauseDecl :: P Decl
pClauseDecl = do
  (n, ps) <- pFunLhs
  r <- pRhs (reservedOp "=")
  ws <- option [] pWhere
  pure (DFun n [Clause ps r ws])

pFunLhs :: P (VarName, [Pat])
pFunLhs = try pInfixLhs <|> pPrefixLhs
  where
    pPrefixLhs = (,) <$> pFunName <*> many pAPat
    -- 被演算子は pAPat ではなく pPat。Haskell の @funlhs -> pat varop pat@
    -- に従い、@Cons x xs ++ ys = ...@ のように括弧なしの構成子適用を
    -- 左右に書けるようにするため。
    pInfixLhs = do
      l <- pPat
      op <- pOpName
      r <- pPat
      pure (VarName op, [l, r])

pFunName :: P VarName
pFunName = varName <|> parens (VarName <$> pOpName)

pRhs :: P () -> P Rhs
pRhs eq = (Plain <$> (eq *> pExpr)) <|> (Guarded <$> some pGuarded)
  where
    pGuarded = do
      reservedOp "|"
      g <- pExpr
      eq
      e <- pExpr
      pure (g, e)

pWhere :: P [Decl]
pWhere = keyword "where" *> (catMaybes <$> blockOf pDecl)

pExpr :: P Expr
pExpr = do
  e <- pOperand
  rs <- many ((,) <$> pOpName <*> pOperand)
  pure (if null rs then e else EOpChain e rs)

pOperand :: P Expr
pOperand = choice [pLam, pLet, pIf, pCase, pApp]

pApp :: P Expr
pApp = foldl1 EApply <$> some pAExpr

pAExpr :: P Expr
pAExpr =
  choice
    [ EVar <$> varName,
      ECon <$> conName,
      ELiteral <$> pLiteral,
      brackets (EList <$> sepBy pExpr (special ',')),
      pParenExpr
    ]
    <?> "式"

pParenExpr :: P Expr
pParenExpr = parens inner
  where
    inner =
      choice
        [ try (ESectionR <$> pOpName <*> pExpr), -- (+ x)
          try (opRef <$> pOpName), -- (+)
          do
            e <- pExpr
            choice
              [ (\op -> ESectionL op e) <$> pOpName, -- (x +)
                ETuple . (e :) <$> some (special ',' *> pExpr),
                pure e
              ]
        ]

opRef :: String -> Expr
opRef op@(':' : _) = ECon (ConName op)
opRef op = EVar (VarName op)

pLam :: P Expr
pLam = do
  reservedOp "\\"
  ps <- some pAPat
  reservedOp "->"
  ELambda ps <$> pExpr

pLet :: P Expr
pLet = do
  keyword "let"
  ds <- catMaybes <$> blockOf pDecl
  keyword "in"
  ELet ds <$> pExpr

pIf :: P Expr
pIf = do
  keyword "if"
  c <- pExpr
  skipMany semi
  keyword "then"
  t <- pExpr
  skipMany semi
  keyword "else"
  EIf c t <$> pExpr

pCase :: P Expr
pCase = do
  keyword "case"
  s <- pExpr
  keyword "of"
  ECase s <$> blockOf pAlt

pAlt :: P Alt
pAlt = do
  p <- pPat
  r <- pRhs (reservedOp "->")
  ws <- option [] pWhere
  pure (Alt p r ws)

pPat :: P Pat
pPat = try (PCon <$> conName <*> some pAPat) <|> pAPat

pAPat :: P Pat
pAPat =
  choice
    [ try (PAs <$> varName <* reservedOp "@" <*> pAPat),
      PVar <$> varName,
      PWild <$ reservedOp "_",
      (\c -> PCon c []) <$> conName,
      PLiteral <$> pLiteral,
      brackets (PList <$> sepBy pPat (special ',')),
      parens $ do
        p <- pPat
        ps <- many (special ',' *> pPat)
        pure (if null ps then p else PTuple (p : ps))
    ]
    <?> "パターン"

groupClauses :: [Decl] -> Either CompileError [Decl]
groupClauses = go []
  where
    go acc [] = Right (reverse acc)
    go acc (DFun n cs : rest) = do
      let (adjacent, rest') = span (isFunOf n) rest
          cs' = cs ++ concatMap clausesOf adjacent
      checkArity n cs'
      if any (isFunOf n) rest'
        then Left (DuplicateDefinition n)
        else go (DFun n cs' : acc) rest'
    go acc (d : rest) = go (d : acc) rest

    isFunOf n (DFun m _) = m == n
    isFunOf _ _ = False

    clausesOf (DFun _ cs) = cs
    clausesOf _ = []

    checkArity n cs = case nub (map clauseArity cs) of
      (a : b : _) -> Left (ArityError (unVarName n) a b)
      _ -> Right ()
