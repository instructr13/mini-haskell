module HS.Loader (loadHS) where

import ApplicativeTRS.Builtin (preludePath, preludeSrc)
import ApplicativeTRS.Check (checkConstructors, fromConViolation)
import ApplicativeTRS.Parser (parseApplicativeTRS)
import Control.Monad.Except (MonadError (throwError))
import HS.Check (checkModule, fromDeclViolation)
import HS.Elab
import HS.InterOp
import HS.Parser (parseHS)
import HS.Signature
import HS.Syntax
import TRS
import TRS.Check (checkTRS)
import TRS.Error
import TRS.Loader (fromViolation)

loadHS :: FilePath -> String -> Either TRSError TRS
loadHS file src = do
  m <- parseHS file src
  prelude <- parseApplicativeTRS preludePath preludeSrc

  let program = appTRSToHS prelude <> m
      sig = mkSignature program
      trs = [elabRuleDecl sig rule | rule <- mRules program]

  case map fromDeclViolation (checkModule sig program)
    ++ map fromConViolation (checkConstructors sig trs)
    ++ map fromViolation (checkTRS trs) of
    [] -> pure trs
    (e : _) -> throwError e
