module HS.Desugar
  ( desugarModule,
    numLit,
    peanoExpr,
    binaryExpr,
    listExpr,
    conApply,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import HS.Error (CompileError (..), Feature (..))
import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse

desugarModule :: Module -> Compile Module
desugarModule = mapM desugarDecl

desugarDecl :: Decl -> Compile Decl
desugarDecl d = case d of
  DData {} -> pure d
  DFixity {} -> pure d
  DFun n cs -> DFun n <$> mapM (desugarClause (unVarName n)) cs

desugarClause :: String -> Clause -> Compile Clause
desugarClause origin (Clause ps rhs ws) = withOrigin origin $ do
  ps' <- mapM desugarPat ps
  body <- desugarRhs rhs
  body' <- desugarExpr body
  ws' <- mapM desugarDecl ws
  wrapped <- wrapWhere ws' body'
  pure (Clause ps' (Plain wrapped) [])

desugarAlt :: Alt -> Compile Alt
desugarAlt (Alt q rhs ws) = do
  q' <- desugarPat q
  body <- desugarRhs rhs
  body' <- desugarExpr body
  ws' <- mapM desugarDecl ws
  wrapped <- wrapWhere ws' body'
  pure (Alt q' (Plain wrapped) [])

wrapWhere :: [Decl] -> Expr -> Compile Expr
wrapWhere [] e = pure e
wrapWhere ds e = do
  on <- asks' layerWhere
  if on then pure (ELet ds e) else unsupported FWhereClause

asks' :: (Layers -> Bool) -> Compile Bool
asks' f = asks (f . envLayers)

-- @| g = e | otherwise = e'@ becomes @case g of {True -> e; False -> e'}@.
--
-- The final guard's condition is dropped only when it is syntactically
-- 'otherwise' or 'True'; dropping it unconditionally would make a non-total
-- final guard silently return the wrong answer instead of failing. Note that
-- guards do not fall through to the next clause: they are desugared here,
-- before pattern matching, which is a documented gap.
desugarRhs :: Rhs -> Compile Expr
desugarRhs (Plain e) = pure e
desugarRhs (Guarded gs) = do
  on <- asks' layerGuards
  if on then build gs else unsupported FGuards
  where
    build [] = failExpr
    build [(g, e)]
      | isAlwaysTrue g = pure e
    build ((g, e) : rest) = do
      alt <- build rest
      boolCase g e alt
    isAlwaysTrue (EVar (VarName "otherwise")) = True
    isAlwaysTrue (ECon (ConName "True")) = True
    isAlwaysTrue _ = False

boolCase :: Expr -> Expr -> Expr -> Compile Expr
boolCase scrut yes no = do
  t <- knownCon KnTrue
  f <- knownCon KnFalse
  pure
    ( ECase
        scrut
        [ Alt (PCon t []) (Plain yes) [],
          Alt (PCon f []) (Plain no) []
        ]
    )

failExpr :: Compile Expr
failExpr = knownRef KnFail

desugarExpr :: Expr -> Compile Expr
desugarExpr = go
  where
    go e = case e of
      EIf c a b -> do
        on <- asks' layerIf
        if on
          then do
            c' <- go c
            a' <- go a
            b' <- go b
            boolCase c' a' b'
          else unsupported FIf
      ESectionL op x -> do
        on <- asks' layerSections
        if on then EApply (opRef op) <$> go x else unsupported FSection
      ESectionR op x -> do
        on <- asks' layerSections
        if on
          then do
            x' <- go x
            y <- freshVar "sec"
            lambdaToLocal [PVar y] (opApply op (EVar y) x')
          else unsupported FSection
      ELambda ps b -> do
        on <- asks' layerLambda
        if on
          then do
            ps' <- mapM desugarPat ps
            b' <- go b
            lambdaToLocal ps' b'
          else unsupported FLambda
      ELiteral l -> desugarLiteral l
      EList xs -> do
        xs' <- mapM go xs
        listExpr xs'
      ETuple xs -> do
        xs' <- mapM go xs
        tupleExpr xs'
      ECon c | Just k <- notationCon c -> knownRef k
      ECase s alts -> ECase <$> go s <*> mapM desugarAlt alts
      ELet ds b -> ELet <$> mapM desugarDecl ds <*> go b
      EOpChain _ _ -> residual "EOpChain" "HS.Fixity"
      _ -> descendExprM go e

-- A lambda becomes a local definition, so it rides exactly the same lifting
-- path as a user-written where/let binding.
lambdaToLocal :: [Pat] -> Expr -> Compile Expr
lambdaToLocal ps b = do
  origin <- asks envOrigin
  n <- VarName <$> freshName (origin ++ "@lam")
  pure (ELet [DFun n [Clause ps (Plain b) []]] (EVar n))

desugarPat :: Pat -> Compile Pat
desugarPat q = case q of
  PWild -> residual "PWild" "HS.Rename"
  PLiteral l -> do
    on <- asks' layerLiterals
    if on then literalPat l else unsupported (litFeature l)
  PList ps -> do
    ps' <- mapM desugarPat ps
    listPat ps'
  PTuple ps -> do
    ps' <- mapM desugarPat ps
    tuplePat ps'
  PAs {} -> unsupported FAsPattern
  POpChain _ _ -> residual "POpChain" "HS.Fixity"
  PCon c ps -> case notationCon c of
    Just k -> PCon <$> knownCon k <*> mapM desugarPat ps
    Nothing -> PCon c <$> mapM desugarPat ps
  PVar _ -> pure q

-- Constructor operators that name a wired-in constructor rather than a
-- user-declared one. @x : xs@ is notation for the list cons, exactly as
-- @[a, b]@ is notation for a chain of them.
notationCon :: ConName -> Maybe KnownName
notationCon (ConName ":") = Just KnCons
notationCon (ConName "[]") = Just KnNil
notationCon _ = Nothing

-- A literal pattern becomes the constructor pattern its value denotes, which
-- is what lets the pattern-match compiler treat it as an ordinary column and
-- makes the overlap of 14-3 disappear.
literalPat :: Literal -> Compile Pat
literalPat (LInt n) = do
  e <- numLit n
  exprToPat e
literalPat (LChar _) = unsupported FCharLiteral
literalPat (LString _) = unsupported FStringLiteral

exprToPat :: Expr -> Compile Pat
exprToPat e = case appSpine e of
  (ECon c, as) -> PCon c <$> mapM exprToPat as
  _ -> internal "literal desugaring produced a non-constructor pattern"

desugarLiteral :: Literal -> Compile Expr
desugarLiteral l = do
  on <- asks' layerLiterals
  if not on
    then unsupported (litFeature l)
    else case l of
      LInt n
        | n < 0 -> unsupported FNegativeLiteral
        | otherwise -> numLit n
      LChar _ -> unsupported FCharLiteral
      LString _ -> unsupported FStringLiteral

litFeature :: Literal -> Feature
litFeature (LInt _) = FNumericLiteral
litFeature (LChar _) = FCharLiteral
litFeature (LString _) = FStringLiteral

numLit :: Integer -> Compile Expr
numLit n = do
  rep <- asks envNumeric
  case rep of
    NumPeano -> do
      z <- knownCon KnZero
      s <- knownCon KnSucc
      pure (peanoExpr z s n)
    NumBinary -> do
      one <- knownCon KnBinOne
      o <- knownCon KnBinO
      i <- knownCon KnBinI
      maybe (throwError (NoLiteralRepresentation NumBinary (show n))) pure (binaryExpr one o i n)

-- num 0 = knZero; num (n+1) = knSucc (num n). Term size n+1.
peanoExpr :: ConName -> ConName -> Integer -> Expr
peanoExpr zero suc = go
  where
    go n
      | n <= 0 = ECon zero
      | otherwise = EApply (ECon suc) (go (n - 1))

-- bin 1 = One; bin (2n) = O (bin n); bin (2n+1) = I (bin n). Term size
-- floor(log2 n)+1. Bin has no representation for zero, hence the Maybe.
binaryExpr :: ConName -> ConName -> ConName -> Integer -> Maybe Expr
binaryExpr one o i = go
  where
    go n
      | n < 1 = Nothing
      | n == 1 = Just (ECon one)
      | even n = EApply (ECon o) <$> go (n `div` 2)
      | otherwise = EApply (ECon i) <$> go (n `div` 2)

listExpr :: [Expr] -> Compile Expr
listExpr xs = do
  nil <- knownCon KnNil
  cons <- knownCon KnCons
  pure (foldr (consWith cons) (ECon nil) xs)

listPat :: [Pat] -> Compile Pat
listPat ps = do
  nil <- knownCon KnNil
  cons <- knownCon KnCons
  pure (foldr (consPatWith cons) (PCon nil []) ps)

tupleExpr :: [Expr] -> Compile Expr
tupleExpr xs = do
  c <- tupleCon (length xs)
  pure (conApply c xs)

tuplePat :: [Pat] -> Compile Pat
tuplePat ps = do
  c <- tupleCon (length ps)
  pure (PCon c ps)

tupleCon :: Int -> Compile ConName
tupleCon n = do
  let c = tupleConName n
  info <- lookupSym (unConName c)
  case info of
    Just i | isConSym i, symArity i == n -> pure c
    _ -> throwError (UnknownConstructor c)

conApply :: ConName -> [Expr] -> Expr
conApply c = foldl EApply (ECon c)

consWith :: ConName -> Expr -> Expr -> Expr
consWith cons x xs = conApply cons [x, xs]

consPatWith :: ConName -> Pat -> Pat -> Pat
consPatWith cons p ps = PCon cons [p, ps]
