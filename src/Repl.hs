{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Repl
  ( loop,
    ReplEnv (..),
    ReplState (..),
    StrategyName (..),
    initState,
    Cmd (..),
    DumpKind (..),
    EvalMode (..),
    parseCmd,
    Reaction (..),
    IORequest (..),
    apply,
    banner,
  )
where

import Control.Exception (SomeException, evaluate, try)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import HS.Check (checkTRS)
import HS.Compile
import HS.Layout (layout)
import HS.Lexer (PosToken (..), SrcPos (..), lexHS)
import HS.Monad (allLayers, coreOnly)
import HS.Name (NumericRep, numericName, numericOfName, signatureFromTRS)
import HS.Pretty (prettyModule)
import Prettyprinter
import Render
import Source
import System.IO
import TRS
import TRS.Parser
import TRS.Pretty

data StrategyName = LeftmostOutermost | LeftmostInnermost | ParallelOutermost
  deriving (Eq, Show, Enum, Bounded)

-- name, spelling shown by :strategy, primary argument word, other accepted words
strategyNames :: [(StrategyName, String, String, [String])]
strategyNames =
  [ (LeftmostOutermost, "leftmost-outermost", "outer", ["outermost", "lo", "o"]),
    (LeftmostInnermost, "leftmost-innermost", "inner", ["innermost", "li", "i"]),
    (ParallelOutermost, "parallel-outermost", "parallel", ["par", "po", "p"])
  ]

describeStrategy :: StrategyName -> String
describeStrategy s =
  case [d | (n, d, _, _) <- strategyNames, n == s] of
    (d : _) -> d
    [] -> show s

strategyOf :: StrategyName -> Strategy
strategyOf LeftmostOutermost = leftmostOutermost
strategyOf LeftmostInnermost = leftmostInnermost
strategyOf ParallelOutermost = parallelOutermost

data ReplEnv = ReplEnv
  { envOut :: Out,
    envErr :: Out
  }

data ReplState = ReplState
  { rsTRS :: TRS,
    rsSource :: Maybe FilePath,
    rsVars :: [String],
    rsStrategy :: StrategyName,
    rsLimit :: Int,
    rsOptions :: Options
  }

initState :: ReplState
initState =
  ReplState
    { rsTRS = [],
      rsSource = Nothing,
      rsVars = [],
      rsStrategy = LeftmostOutermost,
      rsLimit = 10000,
      rsOptions = defaultOptions
    }

data EvalMode = ModeNf | ModeSteps | ModeTrace
  deriving (Eq, Show)

data Cmd
  = CNop
  | CHelp
  | CQuit
  | CLoad FilePath
  | CReload
  | CRules String
  | CVars [String]
  | CStrategy StrategyName
  | CLimit Int
  | CCheck
  | CEval EvalMode String
  | CRun FilePath
  | CDump DumpKind FilePath
  | CCore Bool
  | CNumeric NumericRep
  deriving (Eq, Show)

-- Haskell ソースがどの段階まで来たところを見せるか。
data DumpKind = DumpTokens | DumpStage Stage
  deriving (Eq, Show)

dumpKinds :: [(String, DumpKind)]
dumpKinds = ("tokens", DumpTokens) : [(name, DumpStage s) | (s, name) <- stageNames]

data CmdSpec = CmdSpec
  { csName :: String,
    csArgs :: String,
    csHelp :: String,
    csParse :: String -> Either String Cmd
  }

commands :: [CmdSpec]
commands =
  [ CmdSpec "load" "<file>" "load a .trs or .hs file" (needArg CLoad),
    CmdSpec "reload" "" "reload previously read file" (noArg CReload),
    CmdSpec "rules" "[term]" "show the TRS, pruned to what term can reach" (Right . CRules . trim),
    CmdSpec "vars" "[x y ...]" "view / add input terms' variables" (Right . CVars . words),
    CmdSpec "nf" "<term>" "show the normal form" (needArg (CEval ModeNf)),
    CmdSpec "steps" "<term>" "show steps to compute" (needArg (CEval ModeSteps)),
    CmdSpec "trace" "<term>" "show each steps" (needArg (CEval ModeTrace)),
    CmdSpec "strategy" strategyWordList "change strategy" parseStrategyArg,
    CmdSpec "limit" "<n>" "change limit" parseLimitArg,
    CmdSpec "check" "" "validate current TRS" (noArg CCheck),
    CmdSpec "run" "<file.hs>" "compile a Haskell file and evaluate main" (needArg CRun),
    CmdSpec "dump" (dumpKindList ++ " <file.hs>") "show one pipeline stage of a Haskell file" parseDumpArg,
    CmdSpec "core" "on|off" "with off, disable every desugaring layer" parseCoreArg,
    CmdSpec "numeric" "peano|binary" "numeric literal representation" parseNumericArg,
    CmdSpec "help" "" "show this help" (noArg CHelp),
    CmdSpec "quit" "" "leave the REPL" (noArg CQuit)
  ]

noArg :: Cmd -> String -> Either String Cmd
noArg c a
  | null (trim a) = Right c
  | otherwise = Left "no args"

needArg :: (String -> Cmd) -> String -> Either String Cmd
needArg f a
  | null (trim a) = Left "need args"
  | otherwise = Right (f (trim a))

parseStrategyArg :: String -> Either String Cmd
parseStrategyArg a = case trim a of
  "" -> Left ("specify one of " ++ strategyWordList)
  s -> case [n | (n, _, w, ws) <- strategyNames, s == w || s `elem` ws] of
    (n : _) -> Right (CStrategy n)
    [] -> Left ("unknown strategy: " ++ s ++ " (expected " ++ strategyWordList ++ ")")

strategyWordList :: String
strategyWordList = intercalate "|" [w | (_, _, w, _) <- strategyNames]

parseCoreArg :: String -> Either String Cmd
parseCoreArg a = case trim a of
  "on" -> Right (CCore True)
  "off" -> Right (CCore False)
  s -> Left ("expected on or off, but got " ++ show s)

parseNumericArg :: String -> Either String Cmd
parseNumericArg a = case numericOfName (trim a) of
  Just r -> Right (CNumeric r)
  Nothing -> Left ("expected peano or binary, but got " ++ show (trim a))

dumpKindList :: String
dumpKindList = intercalate "|" (map fst dumpKinds)

parseDumpArg :: String -> Either String Cmd
parseDumpArg a = case break isSpace (trim a) of
  (k, rest)
    | null fp -> Left ("usage: :dump " ++ dumpKindList ++ " <file.hs>")
    | Just kind <- lookup k dumpKinds -> Right (CDump kind fp)
    | otherwise -> Left ("unknown dump kind: " ++ k ++ " (expected " ++ dumpKindList ++ ")")
    where
      fp = trim rest

parseLimitArg :: String -> Either String Cmd
parseLimitArg a = case trim a of
  s
    | not (null s) && all isDigit s -> Right (CLimit (read s))
    | otherwise -> Left "specify natural integer"

parseCmd :: String -> Either String Cmd
parseCmd input = case trim input of
  "" -> Right CNop
  (':' : r) -> parseColon r
  s -> Right (CEval ModeNf s)

parseColon :: String -> Either String Cmd
parseColon r =
  let (name, arg) = break isSpace r
   in case resolveCmd name of
        Left err -> Left err
        Right spec -> csParse spec arg

resolveCmd :: String -> Either String CmdSpec
resolveCmd "" = Left "no command specified (type :help)"
resolveCmd name =
  case [c | c <- commands, name `isPrefixOf` csName c] of
    [c] -> Right c
    [] -> Left ("unknown command: :" ++ name ++ " (type :help)")
    cs -> case filter ((== name) . csName) cs of
      [c] -> Right c
      _ ->
        Left
          ( "ambiguous command: :"
              ++ name
              ++ " -> "
              ++ unwords (map ((':' :) . csName) cs)
          )

data IORequest
  = LoadTRS FilePath
  | RunHS FilePath
  | DumpHS DumpKind FilePath
  | Exit
  deriving (Eq, Show)

data Reaction = Reaction
  { reState :: ReplState,
    reOut :: [Doc Ann],
    reIO :: Maybe IORequest
  }

pure_ :: ReplState -> [Doc Ann] -> Reaction
pure_ st out = Reaction st out Nothing

apply :: ReplState -> Cmd -> Reaction
apply st = \case
  CNop -> pure_ st []
  CHelp -> pure_ st helpLines
  CQuit -> Reaction st [] (Just Exit)
  CLoad fp -> Reaction st [] (Just (LoadTRS fp))
  CReload -> case rsSource st of
    Nothing -> pure_ st [warnDoc "no file loaded yet"]
    Just fp -> Reaction st [] (Just (LoadTRS fp))
  CRules arg
    | null (rsTRS st) -> pure_ st [warnDoc "no rules loaded"]
    | null arg -> pure_ st [prettyTRSNotation haskellNotation (rsTRS st)]
    | otherwise -> case readTerm (rsVars st) arg of
        Left err -> pure_ st [trsErrorDoc err]
        Right t ->
          let cut = prune (rsTRS st) t
           in pure_
                st
                [ setting "rules" (number (length cut) <> "/" <> number (length (rsTRS st))),
                  prettyTRSNotation haskellNotation cut
                ]
  CVars [] -> pure_ st [setting "vars" (varList (rsVars st))]
  CVars vs ->
    let st' = st {rsVars = nub (rsVars st ++ vs)}
     in pure_ st' [setting "vars" (varList (rsVars st'))]
  CStrategy s ->
    pure_
      (st {rsStrategy = s})
      [setting "strategy" (pretty (describeStrategy s))]
  CLimit n -> pure_ (st {rsLimit = n}) [setting "limit" (number n)]
  CCheck ->
    let sig = signatureFromTRS (rsTRS st)
     in pure_ st (violationsDoc (rsTRS st) (checkTRS sig (rsTRS st)))
  CEval mode src -> pure_ st (evalTerm st mode src)
  CRun fp -> Reaction st [] (Just (RunHS fp))
  CDump kind fp -> Reaction st [] (Just (DumpHS kind fp))
  CCore sugar ->
    let st' = withOptions st (\o -> o {optLayers = if sugar then allLayers else coreOnly})
     in pure_ st' [setting "layers" (if sugar then "all" else "core only")]
  CNumeric rep ->
    let st' = withOptions st (\o -> o {optNumeric = rep})
     in pure_ st' [setting "numeric" (pretty (numericName rep))]

-- Leftmost-outermost is the default and the one nf uses, so it gets the
-- zipper; the others go through the generic loop.
normaliseFor :: StrategyName -> Int -> TRS -> Term -> Normalised
normaliseFor LeftmostOutermost = normaliseOutermost
normaliseFor s = normaliseWith (strategyOf s)

withOptions :: ReplState -> (Options -> Options) -> ReplState
withOptions st f = st {rsOptions = f (rsOptions st)}

-- | @name = value@ 形式の設定行。
setting :: Doc Ann -> Doc Ann -> Doc Ann
setting name value = funName name <+> operator "=" <+> value

varList :: [String] -> Doc Ann
varList [] = faint "(none)"
varList vs = hsep (map (varName . pretty) vs)

banner :: Doc Ann
banner =
  vsep
    [ keyword "mini-haskell" <+> faint "— TRS interactive",
      faint "type" <+> keyword ":help" <+> faint "for commands," <+> keyword ":quit" <+> faint "to leave"
    ]

helpLines :: [Doc Ann]
helpLines =
  [ fill colWidth (keyword (pretty (':' : csName c)) <> argsDoc (csArgs c)) <+> pretty (csHelp c)
  | c <- commands
  ]
    ++ [fill colWidth (faint "<term>") <+> "same as :nf"]
  where
    argsDoc "" = mempty
    argsDoc a = space <> faint (pretty a)
    colWidth = 2 + maximum [length (csName c) + length (csArgs c) + 2 | c <- commands]

evalTerm :: ReplState -> EvalMode -> String -> [Doc Ann]
evalTerm st mode src = case readTerm (rsVars st) src of
  Left err -> [trsErrorDoc err]
  Right t -> evalIn st (rsTRS st) mode t

-- Only :trace needs the steps kept, so the other two modes go through the
-- constant-space normaliser.
evalIn :: ReplState -> TRS -> EvalMode -> Term -> [Doc Ann]
evalIn st trs mode t = case mode of
  ModeNf -> case nrOutcome r of
    Normal -> [termDoc (nrTerm r)]
    LimitReached ->
      [ warnDoc ("limit reached after" <+> number (nrSteps r) <+> "steps; stopped at:"),
        termDoc (nrTerm r)
      ]
  ModeSteps -> case nrOutcome r of
    Normal -> [number (nrSteps r)]
    LimitReached -> [number (nrSteps r) <+> faint "(limit reached)"]
  ModeTrace ->
    let tr = traceWith strategy (rsLimit st) trs t
     in (fill stepWidth mempty <> termDoc t)
          : zipWith (renderStep termDoc) [1 :: Int ..] (trSteps tr)
          ++ [warnDoc "limit reached" | trOutcome tr == LimitReached]
  where
    strategy = strategyOf (rsStrategy st)
    r = normaliseFor (rsStrategy st) (rsLimit st) trs t
    termDoc = prettyTermWith haskellNotation (definedSymbols trs)

stepWidth :: Int
stepWidth = 5

renderStep :: (Term -> Doc Ann) -> Int -> Step -> Doc Ann
renderStep termDoc i s =
  fill stepWidth (faint (number i <> "."))
    <> termDoc (stepResult s)
      <+> faint (commaSep (map prettyRedex (stepRedexes s)))

runLoaded :: ReplState -> Either SourceError (TRS, Term) -> [Doc Ann]
runLoaded _ (Left e) = [sourceErrorDoc e]
runLoaded st (Right (trs, body)) = evalIn st trs ModeNf body

dumpSource :: Options -> DumpKind -> FilePath -> String -> [Doc Ann]
dumpSource opts kind fp src = case kind of
  DumpTokens -> case lexHS fp src of
    Left e -> [lexErrorDoc e]
    Right ts -> map tokenLine (layout ts)
  DumpStage s -> case compileToStage opts s fp src of
    Left e -> [compileErrorDoc e]
    Right (RModule m) -> [prettyModule m]
    Right (RArtifact a) -> [prettyTRSNotation haskellNotation (arTRS a)]

tokenLine :: PosToken -> Doc Ann
tokenLine (PosToken (SrcPos l c) t) =
  fill 8 (faint (pretty (show l ++ ":" ++ show c))) <> pretty (show t)

loop :: ReplEnv -> ReplState -> IO ()
loop env st = do
  hPutDoc out (prompt "trs>" <> space)
  hFlush (outHandle out)
  eof <- isEOF
  if eof
    then hPutDocLn out mempty
    else do
      input <- getLine
      case parseCmd input of
        Left err -> do
          hPutDocLn (envErr env) (errorDoc (pretty err))
          loop env st
        Right cmd -> do
          let Reaction st' out' req = apply st cmd
          mapM_ (putDocSafe env) out'
          case req of
            Nothing -> loop env st'
            Just Exit -> return ()
            Just (LoadTRS fp) -> do
              st'' <- loadTRS env fp st'
              loop env st''
            Just (RunHS fp) -> do
              r <- loadMainWith (rsOptions st') fp
              mapM_ (putDocSafe env) (runLoaded st' r)
              loop env st'
            Just (DumpHS kind fp) -> do
              r <- readSource fp
              case r of
                Left e -> hPutDocLn (envErr env) (sourceErrorDoc e)
                Right src -> mapM_ (putDocSafe env) (dumpSource (rsOptions st') kind fp src)
              loop env st'
  where
    out = envOut env

putDocSafe :: ReplEnv -> Doc Ann -> IO ()
putDocSafe env d = do
  r <- try (evaluate (length (renderPlain (outWidth (envOut env)) d)))
  case r of
    Right _ -> hPutDocLn (envOut env) d
    Left e ->
      hPutDocLn
        (envErr env)
        (errorDoc ("exception:" <+> pretty (firstLine (show (e :: SomeException)))))

loadTRS :: ReplEnv -> FilePath -> ReplState -> IO ReplState
loadTRS env fp st =
  do
  r <- loadSourceWith (rsOptions st) fp
  case r of
    Left e -> do
      hPutDocLn (envErr env) (sourceErrorDoc e)
      return st
    Right t -> do
      let vs = trsVariables t
      hPutDocLn
        (envOut env)
        ( okDoc
            ( "loaded"
                <+> number (length t)
                <+> "rules from"
                <+> filepath (pretty fp)
            )
        )
      hPutDocLn (envOut env) (setting "vars" (varList vs))
      return
        st
          { rsTRS = t,
            rsSource = Just fp,
            rsVars = vs
          }

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
