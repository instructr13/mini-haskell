module HS.Compile
  ( Options (..),
    defaultOptions,
    Stage (..),
    StageResult (..),
    Artifact (..),
    compileModule,
    compileWith,
    compileArtifact,
    compileToStage,
    buildDataEnv,
    buildSignature,
    stageNames,
  )
where

import Control.Monad (foldM, unless)
import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (gets, modify)
import qualified Data.Map.Strict as Map
import HS.Check (Violation, checkTRS, describeViolation)
import HS.Defunc (defunctionalize)
import HS.Desugar (desugarModule)
import HS.Error
import HS.Fixity (collectFixities, resolveModule)
import HS.Layout (layout)
import HS.Lift (liftModule)
import HS.Match (matchModule)
import HS.Monad
import HS.Name
import HS.Parser (parseHS)
import HS.Rename (renameModule)
import HS.Syntax
import HS.ToTRS (inlineRelays, toTRS)
import TRS

data Options = Options
  { optNumeric :: NumericRep,
    optLayers :: Layers,
    optLayout :: Bool,
    optInlineRelays :: Bool,
    optCheck :: Bool
  }
  deriving (Eq, Show)

defaultOptions :: Options
defaultOptions =
  Options
    { optNumeric = NumPeano,
      optLayers = allLayers,
      optLayout = True,
      optInlineRelays = True,
      optCheck = True
    }

data Artifact = Artifact
  { arTRS :: TRS,
    arSig :: Signature,
    arViolations :: [Violation]
  }

data Stage
  = SAst
  | SFixity
  | SRename
  | SDesugar
  | SLift
  | SMatch
  | SDefunc
  | STRS
  deriving (Eq, Show, Enum, Bounded)

stageNames :: [(Stage, String)]
stageNames =
  [ (SAst, "ast"),
    (SFixity, "fixity"),
    (SRename, "rename"),
    (SDesugar, "desugar"),
    (SLift, "lift"),
    (SMatch, "match"),
    (SDefunc, "defunc"),
    (STRS, "trs")
  ]

data StageResult
  = RModule Module
  | RArtifact Artifact

compileModule :: FilePath -> String -> Either CompileError TRS
compileModule = compileWith defaultOptions

compileWith :: Options -> FilePath -> String -> Either CompileError TRS
compileWith opts path src = arTRS <$> compileArtifact opts path src

compileArtifact :: Options -> FilePath -> String -> Either CompileError Artifact
compileArtifact opts path src = do
  r <- compileToStage opts STRS path src
  case r of
    RArtifact a -> Right a
    RModule _ -> Left (InternalError "compileToStage STRS returned a module")

compileToStage :: Options -> Stage -> FilePath -> String -> Either CompileError StageResult
compileToStage opts stage path src =
  evalCompile (initialEnv (optNumeric opts) (optLayers opts)) run
  where
    lay = if optLayout opts then layout else id

    -- The module passes through each source-to-source pass in turn; asking
    -- for an earlier stage just stops the fold there.
    passes :: [(Stage, Module -> Compile Module)]
    passes =
      [ (SFixity, resolveFixities),
        (SRename, renameAfterPreparing),
        (SDesugar, desugarModule),
        (SLift, liftModule),
        (SMatch, matchModule),
        -- After Match, because apply's own clauses are non-overlapping and
        -- exhaustive by construction and must keep the shape the spec shows.
        (SDefunc, defunctionalize)
      ]

    run = do
      parsed <- liftEither (parseHS lay path src)
      if stage == SAst
        then pure (RModule parsed)
        else do
          m <- foldUpTo parsed passes
          if stage == STRS then finish m else pure (RModule m)

    foldUpTo :: Module -> [(Stage, Module -> Compile Module)] -> Compile Module
    foldUpTo m [] = pure m
    foldUpTo m ((s, pass) : rest) = do
      m' <- pass m
      if stage == s then pure m' else foldUpTo m' rest

    resolveFixities :: Module -> Compile Module
    resolveFixities m = do
      fixities <- liftEither (collectFixities m)
      liftEither (resolveModule fixities m)

    -- The signature and data environment must exist before anything looks
    -- a name up, and Rename is the first pass that does.
    renameAfterPreparing :: Module -> Compile Module
    renameAfterPreparing m = do
      buildDataEnv m
      buildSignature m
      renameModule m

    finish :: Module -> Compile StageResult
    finish m = do
      raw <- toTRS m
      let trs = if optInlineRelays opts then inlineRelays raw else raw
      sig <- gets stSig
      let violations = checkTRS sig trs
      unless (not (optCheck opts) || null violations) $
        throwError (violationError violations)
      pure (RArtifact (Artifact trs sig violations))

-- From stage 6 on, the pattern-match compiler makes overlap impossible by
-- construction, so a violation here is a compiler bug rather than a problem
-- with the source.
violationError :: [Violation] -> CompileError
violationError vs = InternalError (unlines (map describeViolation vs))

buildDataEnv :: Module -> Compile ()
buildDataEnv m = do
  env <- foldM add Map.empty [(t, cs) | DData t _ cs <- m]
  modify (\s -> s {stData = env})
  where
    add :: DataEnv -> (TyConName, [(ConName, Int)]) -> Compile DataEnv
    add env (t, cs)
      | Map.member t env = throwError (SymbolCollision (unTyConName t))
      | otherwise = pure (Map.insert t cs env)

buildSignature :: Module -> Compile ()
buildSignature m = do
  sequence_ [declareCon t c n | DData t _ cs <- m, (c, n) <- cs]
  sequence_ [declareFun (unVarName n) (funArity cs) | DFun n cs <- m]
