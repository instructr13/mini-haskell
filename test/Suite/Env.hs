module Suite.Env
  ( TestEnv (..),
    withEnv,
    currentStage,
    whenStage,
    ExampleSpec (..),
    expectedExamples,
    discoverExamples,
    compileEither,
    compileOrFail,
    readExample,
  )
where

import Control.Monad (filterM)
import Data.List (isSuffixOf, sort)
import HS.Compile (compileModule)
import HS.Error (CompileError)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import Test.Tasty (TestTree, testGroup)
import TRS (TRS)

data TestEnv = TestEnv
  { envRoot :: FilePath,
    envStage :: Int
  }

defaultStage :: Int
defaultStage = 1

withEnv :: (TestEnv -> TestTree) -> IO TestTree
withEnv k = do
  root <- findRoot "."
  stage <- currentStage
  pure (k (TestEnv root stage))

currentStage :: IO Int
currentStage = maybe defaultStage read <$> lookupEnv "MINI_HASKELL_STAGE"

whenStage :: TestEnv -> Int -> TestTree -> TestTree
whenStage env n t
  | envStage env >= n = t
  | otherwise = testGroup (nameOf t ++ " [skipped: needs stage " ++ show n ++ "]") []
  where
    nameOf _ = "stage " ++ show n

findRoot :: FilePath -> IO FilePath
findRoot d = do
  here <- doesFileExist (d </> "package.yaml")
  if here then pure d else pure d

data ExampleSpec = ExampleSpec
  { exPath :: FilePath,
    exStage :: Int
  }
  deriving (Eq, Show)

expectedExamples :: [ExampleSpec]
expectedExamples =
  [ ExampleSpec "examples/Prelude/Base.hs" 1,
    ExampleSpec "examples/Prelude/HO.hs" 7,
    ExampleSpec "examples/Prelude/Num.hs" 8,
    ExampleSpec "examples/addall.hs" 5,
    ExampleSpec "examples/alpha.hs" 5,
    ExampleSpec "examples/colonlist.hs" 6,
    ExampleSpec "examples/demand.hs" 4,
    ExampleSpec "examples/firsttwo.hs" 6,
    ExampleSpec "examples/headof.hs" 6,
    ExampleSpec "examples/clos-chain.hs" 7,
    ExampleSpec "examples/lambda-map.hs" 7,
    ExampleSpec "examples/litpat.hs" 8,
    ExampleSpec "examples/map.hs" 7,
    ExampleSpec "examples/qsort-explicit.hs" 4,
    ExampleSpec "examples/qsort.hs" 8,
    ExampleSpec "examples/shortcircuit.hs" 2,
    ExampleSpec "examples/simple.hs" 2,
    ExampleSpec "examples/wildcard.hs" 6
  ]

discoverExamples :: TestEnv -> IO [FilePath]
discoverExamples env = sort <$> go (envRoot env </> "examples")
  where
    go d = do
      ok <- doesDirectoryExist d
      if not ok
        then pure []
        else do
          names <- listDirectory d
          let paths = [d </> n | n <- names]
          files <- filterM doesFileExist paths
          dirs <- filterM doesDirectoryExist paths
          nested <- concat <$> mapM go dirs
          pure ([f | f <- files, ".hs" `isSuffixOf` f] ++ nested)

readExample :: TestEnv -> FilePath -> IO String
readExample env rel = readFile (envRoot env </> rel)

compileEither :: TestEnv -> FilePath -> IO (Either CompileError TRS)
compileEither env rel = do
  src <- readExample env rel
  pure (compileModule rel src)

compileOrFail :: TestEnv -> FilePath -> IO TRS
compileOrFail env rel = do
  r <- compileEither env rel
  either (\e -> fail (rel ++ ": " ++ show e)) pure r
