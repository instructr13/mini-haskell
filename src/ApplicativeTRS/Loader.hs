module ApplicativeTRS.Loader (loadApplicativeTRS) where

import ApplicativeTRS.Elab
import ApplicativeTRS.Parser (parseApplicativeTRS)
import ApplicativeTRS.Syntax
import Control.Monad.Except (MonadError (throwError))
import TRS
import TRS.Check (checkTRS)
import TRS.Error
import TRS.Loader

loadApplicativeTRS :: FilePath -> String -> Either TRSError TRS
loadApplicativeTRS file src = do
  sectionSet <- parseApplicativeTRS file src

  let vars = ssVars sectionSet
  let trs = convert vars [elabRule vars rule | rule <- ssRules sectionSet]

  case checkTRS trs of
    [] -> pure trs
    (v : _) -> throwError (fromViolation v)
