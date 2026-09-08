{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HS.Monad (module HS.Monad) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import HS.Error
import HS.Name
import HS.Syntax
import HS.Traverse
import TRS

type DataEnv = Map TyConName [(ConName, Int)]

-- Which desugaring layers are switched on. --core-only turns them all off,
-- which is the only automatic check that the sugar really is removable.
data Layers = Layers
  { layerLiterals :: Bool,
    layerGuards :: Bool,
    layerIf :: Bool,
    layerWhere :: Bool,
    layerSections :: Bool,
    layerLambda :: Bool
  }
  deriving (Eq, Show)

allLayers :: Layers
allLayers = Layers True True True True True True

coreOnly :: Layers
coreOnly = Layers False False False False False False

data Env = Env
  { envNumeric :: NumericRep,
    envLayers :: Layers,
    -- The binders in scope, in positional order. The order of this list is
    -- what fixes the order of a case's extra arguments and of a lifted
    -- definition's captured arguments, so it must never come from a Set.
    envScope :: [VarName],
    envOrigin :: String
  }

data St = St
  { -- One counter per base name, so that split#1 and split#2 come out with
    -- those numbers no matter how much unrelated code ran first, and adding
    -- a function at the top of a file does not renumber everything below.
    stFresh :: !(Map String Int),
    stSig :: Signature,
    stData :: DataEnv,
    -- Top-level declarations produced by a pass (lifted local functions,
    -- defunctionalisation's apply) that still have to go through the rest of
    -- the pipeline.
    stPending :: [Decl],
    stTRS :: [Rule]
  }

newtype Compile a = Compile (ReaderT Env (StateT St (Either CompileError)) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState St,
      MonadError CompileError
    )

initialEnv :: NumericRep -> Layers -> Env
initialEnv rep layers =
  Env {envNumeric = rep, envLayers = layers, envScope = [], envOrigin = "main"}

initialSt :: St
initialSt =
  St
    { stFresh = Map.empty,
      stSig = Map.empty,
      stData = Map.empty,
      stPending = [],
      stTRS = []
    }

runCompile :: Env -> St -> Compile a -> Either CompileError (a, St)
runCompile env st (Compile m) = runStateT (runReaderT m env) st

evalCompile :: Env -> Compile a -> Either CompileError a
evalCompile env m = fst <$> runCompile env initialSt m

-- A generated name always contains '#', and the lexer accepts '#' in no
-- identifier and no digit in an operator, so base#n can never be written in
-- a source file: a symbol contains '#' exactly when the compiler made it.
freshName :: String -> Compile String
freshName base = do
  n <- gets (succ . Map.findWithDefault 0 base . stFresh)
  modify (\s -> s {stFresh = Map.insert base n (stFresh s)})
  pure (base ++ "#" ++ show n)

freshVar :: String -> Compile VarName
freshVar base = VarName <$> freshName base

freshFun :: String -> Int -> Compile String
freshFun base arity = do
  name <- freshName base
  declareFun name arity
  pure name

freshCon :: TyConName -> String -> Int -> Compile ConName
freshCon ty base arity = do
  c <- ConName <$> freshName base
  declareCon ty c arity
  pure c

lookupSym :: String -> Compile (Maybe SymInfo)
lookupSym name = gets (Map.lookup name . stSig)

requireSym :: String -> Compile SymInfo
requireSym name = do
  info <- lookupSym name
  maybe (throwError (UnknownVariable (VarName name))) pure info

arityOf :: String -> Compile Int
arityOf name = symArity <$> requireSym name

isTopLevel :: VarName -> Compile Bool
isTopLevel (VarName v) = gets (Map.member v . stSig)

declareFun :: String -> Int -> Compile ()
declareFun name arity = declare name (SymInfo arity FunSym)

declareCon :: TyConName -> ConName -> Int -> Compile ()
declareCon ty (ConName name) arity = declare name (SymInfo arity (ConSym ty))

declare :: String -> SymInfo -> Compile ()
declare name = declareWith (throwError (SymbolCollision name)) name

-- Redeclaration is fine when the entry is identical; passes that re-emit a
-- symbol they already declared go through this.
declareIfAbsent :: String -> SymInfo -> Compile ()
declareIfAbsent = declareWith (pure ())

declareWith :: Compile () -> String -> SymInfo -> Compile ()
declareWith onClash name info = do
  seen <- lookupSym name
  case seen of
    Just _ -> onClash
    Nothing -> modifySig (Map.insert name info)

modifySig :: (Signature -> Signature) -> Compile ()
modifySig f = modify (\s -> s {stSig = f (stSig s)})

typeOfCon :: ConName -> Compile TyConName
typeOfCon c = do
  info <- lookupSym (unConName c)
  case info of
    Just i | ConSym ty <- symKind i -> pure ty
    _ -> throwError (UnknownConstructor c)

constructorsOf :: TyConName -> Compile [(ConName, Int)]
constructorsOf ty = gets (Map.findWithDefault [] ty . stData)

siblingsOf :: ConName -> Compile [(ConName, Int)]
siblingsOf c = do
  ty <- typeOfCon c
  constructorsOf ty

-- Wired-in names are resolved on demand, so a module pays nothing for the
-- ones it never uses -- which is what makes the spec's own test sources,
-- most of which declare only one data type, compilable.
knownCon :: KnownName -> Compile ConName
knownCon k = do
  found <- mapM lookupSym names
  case [ConName n | (n, Just i) <- zip names found, isConSym i, symArity i == arity] of
    (c : _) -> pure c
    [] -> case [(n, i) | (n, Just i) <- zip names found] of
      ((n, i) : _) -> throwError (KnownNameMismatch k n arity (symArity i))
      [] -> throwError (MissingKnownName k (knownName k) arity)
  where
    (names, arity) = knownSpec k

knownRef :: KnownName -> Compile Expr
knownRef k = ECon <$> knownCon k

-- A top-level declaration produced by a pass, to be run through the rest of
-- the pipeline.
emitDecl :: Decl -> Compile ()
emitDecl d = modify (\s -> s {stPending = d : stPending s})

takeDecls :: Compile [Decl]
takeDecls = do
  ds <- gets stPending
  modify (\s -> s {stPending = []})
  pure (reverse ds)

emitRule :: Rule -> Compile ()
emitRule rule = modify (\s -> s {stTRS = rule : stTRS s})

emitted :: Compile TRS
emitted = gets (reverse . stTRS)

withOrigin :: String -> Compile a -> Compile a
withOrigin name = local (\e -> e {envOrigin = name})

-- Appends binders to the positional scope.
withScope :: [VarName] -> Compile a -> Compile a
withScope vs = local (\e -> e {envScope = envScope e ++ vs})

-- Replaces one binder in place. Entering a case alternative on a variable
-- must substitute the alternative's binders for that variable *at its
-- position*, not append them: that is what makes the extra-argument vector
-- come out as the spec's worked examples require.
replaceScope :: VarName -> [VarName] -> Compile a -> Compile a
replaceScope v vs = local (\e -> e {envScope = go (envScope e)})
  where
    go [] = vs
    go (w : ws)
      | w == v = vs ++ ws
      | otherwise = w : go ws

-- The one place the extra-argument vector is computed, shared by
-- compileCase's v-bar and Lift's w-bar so they cannot drift apart.
capturedVars :: (FreeVarsOf a) => [VarName] -> a -> Compile [VarName]
capturedVars excluded x = do
  scope <- asks envScope
  sig <- gets stSig
  let used = Set.fromList (freeVars x) `Set.difference` Set.fromList excluded
  pure [v | v <- scope, Set.member v used, not (Map.member (unVarName v) sig)]

unsupported :: Feature -> Compile a
unsupported = throwError . Unsupported

internal :: String -> Compile a
internal = throwError . InternalError

-- A node that a pass should have eliminated. Naming the pass turns "something
-- is wrong deep in ToTRS" into "HS.Desugar has a bug".
residual :: String -> String -> Compile a
residual node pass = internal (node ++ " survived " ++ pass)
