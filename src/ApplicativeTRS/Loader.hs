module ApplicativeTRS.Loader (loadApplicativeTRS) where

import ApplicativeTRS.Builtin (preludePath, preludeSrc)
import ApplicativeTRS.Check (checkConstructors, fromConViolation)
import ApplicativeTRS.Elab
import ApplicativeTRS.Parser (parseApplicativeTRS)
import ApplicativeTRS.Signature
import ApplicativeTRS.Syntax
import ApplicativeTRS.Wild
import Control.Monad.Except (MonadError (throwError))
import TRS
import TRS.Check (checkTRS)
import TRS.Error
import TRS.Loader (fromViolation)

loadApplicativeTRS :: FilePath -> String -> Either TRSError TRS
loadApplicativeTRS file src = do
  m <- parseApplicativeTRS file src
  prelude <- parseApplicativeTRS preludePath preludeSrc
  program <- expandAppModule (prelude <> m)

  let sig = mkSignature program
      trs = [elabRule sig rule | rule <- amRules program]

  case map fromConViolation (checkConstructors sig trs)
    ++ map fromViolation (checkTRS trs) of
    [] -> pure trs
    (e : _) -> throwError e
