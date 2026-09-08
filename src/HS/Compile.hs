{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HS.Compile (module HS.Compile) where

import Control.Monad (foldM, unless, when)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HS.Check (Violation (..), checkTRS)
import HS.Error
import HS.Fixity (collectFixities, resolveModule)
import HS.Lexer (PosToken)
import HS.Name
import HS.Parser (parseHS)
import HS.Syntax
import TRS

type DataEnv = Map TyConName [(ConName, Int)]

data Env = Env {envSig :: Signature, envData :: DataEnv, envKnown :: Known}

data St = St {stFresh :: !Int, stTRS :: TRS}

newtype Compile a = Compile (ReaderT Env (StateT St (Either CompileError)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState St,
      MonadError CompileError
    )

freshSym :: String -> Compile String
freshSym sym = do
  c <- gets stFresh
  modify (\s -> s {stFresh = c + 1})

  return (sym ++ "#" ++ show c)

emitRule :: Rule -> Compile ()
emitRule rule = do
  modify (\s -> s {stTRS = rule : stTRS s})

compile :: FilePath -> String -> Compile ()
compile = compileWith id

compileWith :: ([PosToken] -> [PosToken]) -> FilePath -> String -> Compile ()
compileWith lay path src = do
  parsed <- liftEither (parseHS lay path src)
  fixities <- liftEither (collectFixities parsed)
  m <- liftEither (resolveModule fixities parsed)
  dataEnv <- buildDataEnv m
  sig <- buildSignature m
  known <- either (throwError . UnknownKnownName) pure (resolveKnown sig)
  local (const (Env sig dataEnv known)) $ do
    mapM_ compileDecl m
    gets (reverse . stTRS) >>= checkRules sig

runCompile :: Compile a -> Either CompileError (a, St)
runCompile (Compile m) = runStateT (runReaderT m seedEnv) seedSt
  where
    seedEnv = Env {envSig = Map.empty, envData = Map.empty, envKnown = defaultKnown}
    seedSt = St {stFresh = 0, stTRS = []}

compileModule :: FilePath -> String -> Either CompileError TRS
compileModule path src = reverse . stTRS . snd <$> runCompile (compile path src)

buildDataEnv :: Module -> Compile DataEnv
buildDataEnv m = foldM add Map.empty [(t, cs) | DData t _ cs <- m]
  where
    add :: DataEnv -> (TyConName, [(ConName, Int)]) -> Compile DataEnv
    add env (t, cs)
      | Map.member t env = throwError (SymbolCollision (unTyConName t))
      | otherwise = pure (Map.insert t cs env)

buildSignature :: Module -> Compile Signature
buildSignature m = foldM add Map.empty (cons ++ funs)
  where
    cons =
      [ (unConName c, SymInfo {symArity = n, symKind = ConSym t})
      | DData t _ cs <- m,
        (c, n) <- cs
      ]
    funs =
      [ (unVarName n, SymInfo {symArity = arity cs, symKind = FunSym})
      | DFun n cs <- m
      ]
    arity [] = 0
    arity (c : _) = clauseArity c
    add :: Signature -> (String, SymInfo) -> Compile Signature
    add sig (name, info)
      | Map.member name sig = throwError (SymbolCollision name)
      | otherwise = pure (Map.insert name info sig)

lookupSym :: String -> Compile (Maybe SymInfo)
lookupSym name = asks (Map.lookup name . envSig)

compileDecl :: Decl -> Compile ()
compileDecl d = case d of
  DData {} -> pure ()
  DFixity {} -> pure ()
  DFun n cs -> compileFun n cs

compileFun :: VarName -> [Clause] -> Compile ()
compileFun n = mapM_ (\c -> clauseRule n c >>= emitRule)

clauseRule :: VarName -> Clause -> Compile Rule
clauseRule n (Clause ps rhs ws) = do
  unless (null ws) (unsupported "where clause")
  body <- case rhs of
    Plain e -> pure e
    Guarded _ -> unsupported "guards"
  (args, bound) <- patTerms ps
  r <- exprTerm bound body
  pure (F (unVarName n) args, r)

-- パターン列を項に落とし、束縛される変数名を集める。左線形性のような
-- 規則の健全性は checkRules がまとめて見るので、ここでは組み立てるだけ。
patTerms :: [Pat] -> Compile ([Term], [String])
patTerms ps = do
  results <- mapM patTerm ps
  pure (map fst results, concatMap snd results)

patTerm :: Pat -> Compile (Term, [String])
patTerm p = case p of
  PVar v -> pure (V (unVarName v), [unVarName v])
  PWild -> do
    x <- freshSym "_"
    pure (V x, [x])
  PCon c ps -> do
    checkConArity c (length ps)
    sub <- mapM patTerm ps
    pure (F (unConName c) (map fst sub), concatMap snd sub)
  PLiteral l -> do
    t <- literalTerm l
    pure (t, [])
  PList ps -> do
    sub <- mapM patTerm ps
    k <- asks envKnown
    pure (listTerm k (map fst sub), concatMap snd sub)
  PTuple ps -> do
    sub <- mapM patTerm ps
    t <- tupleTerm (map fst sub)
    pure (t, concatMap snd sub)
  PAs _ _ -> unsupported "as pattern (x@p)"

exprTerm :: [String] -> Expr -> Compile Term
exprTerm bound = go
  where
    go e = case e of
      EVar v
        | unVarName v `elem` bound -> pure (V (unVarName v))
        | otherwise -> applySym (unVarName v) []
      ECon c -> applyCon c []
      ELiteral l -> literalTerm l
      EList es -> do
        k <- asks envKnown
        listTerm k <$> mapM go es
      ETuple es -> mapM go es >>= tupleTerm
      EApply _ _ -> case appSpine e of
        (EVar v, as)
          | unVarName v `elem` bound -> unsupported "higher-order application"
          | otherwise -> mapM go as >>= applySym (unVarName v)
        (ECon c, as) -> mapM go as >>= applyCon c
        _ -> unsupported "application of a non-symbol"
      EOpChain _ _ -> unsupported "unresolved operator chain"
      ESectionL _ _ -> unsupported "operator section"
      ESectionR _ _ -> unsupported "operator section"
      ELambda _ _ -> unsupported "lambda"
      ELet _ _ -> unsupported "let"
      EIf _ _ _ -> unsupported "if"
      ECase _ _ -> unsupported "case"

applySym :: String -> [Term] -> Compile Term
applySym name args = do
  info <- lookupSym name
  case info of
    Nothing -> throwError (UnknownVariable (VarName name))
    Just i -> do
      checkArity name (symArity i) (length args)
      pure (F name args)

applyCon :: ConName -> [Term] -> Compile Term
applyCon c args = do
  checkConArity c (length args)
  pure (F (unConName c) args)

checkConArity :: ConName -> Int -> Compile ()
checkConArity c n = do
  info <- lookupSym (unConName c)
  case info of
    Just i | isConSym i -> checkArity (unConName c) (symArity i) n
    _ -> throwError (UnknownConstructor c)

checkArity :: String -> Int -> Int -> Compile ()
checkArity name expected actual =
  when (expected /= actual) (throwError (ArityError name expected actual))

literalTerm :: Literal -> Compile Term
literalTerm (LInt n)
  | n < 0 = unsupported "negative integer literal"
  | otherwise = do
      k <- asks envKnown
      pure (peano k n)
literalTerm (LChar _) = unsupported "character literal"
literalTerm (LString _) = unsupported "string literal"

peano :: Known -> Integer -> Term
peano k = go
  where
    zero = F (unConName (knZero k)) []
    go n
      | n <= 0 = zero
      | otherwise = F (unConName (knSucc k)) [go (n - 1)]

listTerm :: Known -> [Term] -> Term
listTerm k = foldr (\x xs -> F (unConName (knCons k)) [x, xs]) (F (unConName (knNil k)) [])

tupleTerm :: [Term] -> Compile Term
tupleTerm ts = do
  k <- asks envKnown
  let c = knTuple k (length ts)
  info <- lookupSym (unConName c)
  case info of
    Just i | isConSym i, symArity i == length ts -> pure (F (unConName c) ts)
    _ -> throwError (UnknownKnownName (unConName c))

checkRules :: Signature -> TRS -> Compile ()
checkRules sig trs = case checkTRS sig trs of
  [] -> pure ()
  (v : _) -> throwError (fromViolation v)

fromViolation :: Violation -> CompileError
fromViolation v = case v of
  RootOverlap r _ -> case ruleFun r of
    Just n -> OverlappingClauses n
    Nothing -> UnsupportedFeature ("overlapping rules: " ++ showRule r)
  NonLeftLinear r x ->
    UnsupportedFeature ("non-linear pattern (" ++ x ++ ") in " ++ showRule r)
  UnboundRhsVar _ x -> UnknownVariable (VarName x)
  ArityMismatch f m n -> ArityError f m n
  LhsIsVariable r -> UnsupportedFeature ("variable left-hand side: " ++ showRule r)
  NotConstructorSystem r ->
    UnsupportedFeature ("not a constructor system: " ++ showRule r)

ruleFun :: Rule -> Maybe VarName
ruleFun (F f _, _) = Just (VarName f)
ruleFun _ = Nothing

unsupported :: String -> Compile a
unsupported = throwError . UnsupportedFeature
