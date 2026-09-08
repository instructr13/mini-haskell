{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- AI-Generated REPL

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
    inferSignature,
  )
where

import Control.Exception (SomeException, evaluate, try)
import Data.Char (isDigit, isSpace)
import Data.List (intercalate, isPrefixOf, nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import HS.Check (checkTRS)
import HS.Compile (compileModule)
import HS.Lexer (PosToken (..), SrcPos (..), lexHS)
import HS.Name
  ( Signature,
    SymInfo (..),
    SymKind (..),
    TyConName (..),
  )
import HS.Parser (parseHS)
import HS.Pretty (prettyModule)
import Prettyprinter
import Render
import Source
import System.IO
import TRS
import TRS.Parser
import TRS.Pretty

trsRules :: TRS -> [Rule]
trsRules = id

ruleParts :: Rule -> (Term, Term)
ruleParts = id

ruleLhs :: Rule -> Term
ruleLhs = fst . ruleParts

data StrategyName = Outermost
  deriving (Eq, Show)

describeStrategy :: StrategyName -> String
describeStrategy Outermost = "outermost-parallel"

strategyOf :: StrategyName -> Strategy
strategyOf Outermost = rewrite

data ReplEnv = ReplEnv
  { envOut :: Out,
    envErr :: Out
  }

data ReplState = ReplState
  { rsTRS :: TRS,
    rsSource :: Maybe FilePath,
    rsVars :: [String],
    rsSig :: Maybe Signature,
    rsStrategy :: StrategyName,
    rsLimit :: Int
  }

initState :: ReplState
initState =
  ReplState
    { rsTRS = [],
      rsSource = Nothing,
      rsVars = [],
      rsSig = Nothing,
      rsStrategy = Outermost,
      rsLimit = 10000
    }

data EvalMode = ModeNf | ModeSteps | ModeTrace
  deriving (Eq, Show)

data Cmd
  = CNop
  | CHelp
  | CQuit
  | CLoad FilePath
  | CReload
  | CRules
  | CVars [String]
  | CStrategy StrategyName
  | CLimit Int
  | CCheck
  | CEval EvalMode String
  | CRun FilePath
  | CDump DumpKind FilePath
  deriving
    ( -- | CCore Bool -- :core on|off
      Eq,
      Show
    )

-- Haskell ソースがどの段階まで来たところを見せるか。
data DumpKind = DumpTokens | DumpAst | DumpTRS
  deriving (Eq, Show)

dumpKinds :: [(String, DumpKind)]
dumpKinds = [("tokens", DumpTokens), ("ast", DumpAst), ("trs", DumpTRS)]

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
    CmdSpec "rules" "" "show current TRS" (noArg CRules),
    CmdSpec "vars" "[x y ...]" "view / add input terms' variables" (Right . CVars . words),
    CmdSpec "nf" "<term>" "show the normal form" (needArg (CEval ModeNf)),
    CmdSpec "steps" "<term>" "show steps to compute" (needArg (CEval ModeSteps)),
    CmdSpec "trace" "<term>" "show each steps" (needArg (CEval ModeTrace)),
    CmdSpec "strategy" "outer" "change strategy" parseStrategyArg,
    CmdSpec "limit" "<n>" "change limit" parseLimitArg,
    CmdSpec "check" "" "validate current TRS" (noArg CCheck),
    CmdSpec "run" "<file.hs>" "compile a Haskell file and evaluate main" (needArg CRun),
    CmdSpec "dump" "<kind> <file.hs>" "show tokens / ast / trs of a Haskell file" parseDumpArg,
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
  "" -> Left "specify outer"
  s
    | s `elem` ["outer", "outermost", "o"] -> Right (CStrategy Outermost)
    | otherwise -> Left ("unknown strategy: " ++ s)

parseDumpArg :: String -> Either String Cmd
parseDumpArg a = case break isSpace (trim a) of
  (k, rest)
    | null fp -> Left ("usage: :dump " ++ kindList ++ " <file.hs>")
    | Just kind <- lookup k dumpKinds -> Right (CDump kind fp)
    | otherwise -> Left ("unknown dump kind: " ++ k ++ " (expected " ++ kindList ++ ")")
    where
      fp = trim rest
  where
    kindList = intercalate "|" (map fst dumpKinds)

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
   in either Left (\spec -> csParse spec arg) (resolveCmd name)

resolveCmd :: String -> Either String CmdSpec
resolveCmd "" = Left "no command specified (type :help)"
resolveCmd name =
  case filter (\c -> name `isPrefixOf` csName c) commands of
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
  CRules
    | null (rsTRS st) -> pure_ st [warnDoc "no rules loaded"]
    | otherwise -> pure_ st [prettyTRS (rsTRS st)]
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
    let sig = fromMaybe (inferSignature (rsTRS st)) (rsSig st)
     in pure_ st (renderViolations (checkTRS sig (rsTRS st)))
  CEval mode src -> pure_ st (evalTerm st mode src)
  CRun fp -> Reaction st [] (Just (RunHS fp))
  CDump kind fp -> Reaction st [] (Just (DumpHS kind fp))

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

evalIn :: ReplState -> TRS -> EvalMode -> Term -> [Doc Ann]
evalIn st trs mode t =
  let (steps, outcome) = traceLimit (strategyOf (rsStrategy st)) (rsLimit st) trs t
      termDoc = prettyTermWith (definedSymbols trs)
   in case mode of
        ModeNf ->
          let (final, n) = walk t 0 steps
           in case outcome of
                Normal -> [termDoc final]
                LimitReached ->
                  [ warnDoc ("limit reached after" <+> number n <+> "steps; stopped at:"),
                    termDoc final
                  ]
        ModeSteps ->
          let (_, n) = walk t 0 steps
           in case outcome of
                Normal -> [number n]
                LimitReached -> [number n <+> faint "(limit reached)"]
        ModeTrace ->
          (fill stepWidth mempty <> termDoc t)
            : zipWith (renderStep termDoc) [1 :: Int ..] steps
            ++ [warnDoc "limit reached" | outcome == LimitReached]

stepWidth :: Int
stepWidth = 5

renderStep :: (Term -> Doc Ann) -> Int -> Term -> Doc Ann
renderStep termDoc i s =
  fill stepWidth (faint (number i <> ".")) <> termDoc s

runSource :: ReplState -> FilePath -> String -> [Doc Ann]
runSource st fp src = case compileModule fp src of
  Left e -> [compileErrorDoc e]
  Right trs -> case lookup (F "main" []) trs of
    Nothing ->
      [errorDoc ("no rule for" <+> funName "main" <+> "in" <+> filepath (pretty fp))]
    Just body -> evalIn st trs ModeNf body

dumpSource :: DumpKind -> FilePath -> String -> [Doc Ann]
dumpSource kind fp src = case kind of
  DumpTokens -> case lexHS fp src of
    Left e -> [lexErrorDoc e]
    Right ts -> map tokenLine ts
  -- HS.Layout が入るまでレイアウトは素通し。
  DumpAst -> case parseHS id fp src of
    Left e -> [compileErrorDoc e]
    Right m -> [prettyModule m]
  DumpTRS -> case compileModule fp src of
    Left e -> [compileErrorDoc e]
    Right trs -> [prettyTRS trs]

tokenLine :: PosToken -> Doc Ann
tokenLine (PosToken (SrcPos l c) t) =
  fill 8 (faint (pretty (show l ++ ":" ++ show c))) <> pretty (show t)

walk :: Term -> Int -> [Term] -> (Term, Int)
walk t !k [] = (t, k)
walk _ !k (u : us) = walk u (k + 1) us

renderViolations :: (Show a) => [a] -> [Doc Ann]
renderViolations [] = [okDoc "no violations"]
renderViolations vs =
  errorDoc (number (length vs) <+> "violation(s):")
    : [indent 2 (pretty (show v)) | v <- vs]

inferSignature :: TRS -> Signature
inferSignature rs =
  M.fromListWith
    keepFirst
    [(f, SymInfo n (kindOf f)) | (f, n) <- occurrences]
  where
    rules' = trsRules rs
    occurrences =
      concatMap
        ( \r ->
            let (l, x) = ruleParts r
             in symbolOccurrences l ++ symbolOccurrences x
        )
        rules'
    defined = [f | r <- rules', F f _ <- [ruleLhs r]]
    kindOf f
      | f `elem` defined = FunSym
      | otherwise = ConSym (TyConName "?")
    keepFirst _new old = old

collectVars :: TRS -> [String]
collectVars = nub . concatMap (variables . ruleLhs) . trsRules

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
        Left err -> hPutDocLn (envErr env) (errorDoc (pretty err)) >> loop env st
        Right cmd -> do
          let Reaction st' out' req = apply st cmd
          mapM_ (putDocSafe env) out'
          case req of
            Nothing -> loop env st'
            Just Exit -> return ()
            Just (LoadTRS fp) -> loadTRS env fp st' >>= loop env
            Just (RunHS fp) -> do
              readSourceFile env fp () (mapM_ (putDocSafe env) . runSource st' fp)
              loop env st'
            Just (DumpHS kind fp) -> do
              readSourceFile env fp () (mapM_ (putDocSafe env) . dumpSource kind fp)
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

-- 読めなければ標準エラーに出して fallback を返す。
readSourceFile :: ReplEnv -> FilePath -> a -> (String -> IO a) -> IO a
readSourceFile env fp fallback k = do
  r <- try (readFile fp) :: IO (Either SomeException String)
  case r of
    Left e -> do
      hPutDocLn
        (envErr env)
        ( errorDoc
            ( "cannot read"
                <+> filepath (pretty fp)
                <> ":"
                <+> pretty (firstLine (show (e :: SomeException)))
            )
        )
      return fallback
    Right src -> k src

loadTRS :: ReplEnv -> FilePath -> ReplState -> IO ReplState
loadTRS env fp st = readSourceFile env fp st $ \src -> case sourceTRS fp src of
  Left e -> do
    hPutDocLn (envErr env) (sourceErrorDoc e)
    return st
  Right t -> do
    let vs = collectVars t
    hPutDocLn
      (envOut env)
      ( okDoc
          ( "loaded"
              <+> number (length (trsRules t))
              <+> "rules from"
              <+> filepath (pretty fp)
          )
      )
    hPutDocLn (envOut env) (setting "vars" (varList vs))
    return
      st
        { rsTRS = t,
          rsSource = Just fp,
          rsVars = vs,
          rsSig = Nothing
        }

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
