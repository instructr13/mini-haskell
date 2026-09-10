module HS.Loader (loadHS) where

import ApplicativeTRS.Builtin (preludePath, preludeSrc)
import ApplicativeTRS.Check (checkConstructors, fromConViolation)
import qualified ApplicativeTRS.Elab as AS
import ApplicativeTRS.Parser (parseApplicativeTRS)
import qualified ApplicativeTRS.Signature as AS
import qualified ApplicativeTRS.Syntax as AS
import ApplicativeTRS.Wild (expandAppModule)
import Control.Monad.Except (MonadError (throwError))
import HS.Check (checkModule, fromDeclViolation)
import HS.Elab
import HS.Parser (parseHS)
import HS.Signature
import HS.Syntax
import HS.Wild
import TRS
import TRS.Check (checkTRS)
import TRS.Error
import TRS.Loader (fromViolation)

-- The prelude is written in applicative TRS syntax
loadPrelude :: Either TRSError (AS.Signature, TRS)
loadPrelude = do
  m <- parseApplicativeTRS preludePath preludeSrc
  program <- expandAppModule m

  let sig = AS.mkSignature program

  pure (sig, [AS.elabRule sig rule | rule <- AS.amRules program])

loadHS :: FilePath -> String -> Either TRSError TRS
loadHS file src = do
  (preludeSig, preludeTrs) <- loadPrelude
  m <- parseHS file src

  let program = expandModule m
      sig = preludeSig <> mkSignature program
      trs = preludeTrs ++ [elabRuleDecl sig rule | rule <- mRules program]

  case [fromDeclViolation v | v <- checkModule sig program]
    ++ [fromConViolation v | v <- checkConstructors sig trs]
    ++ [fromViolation v | v <- checkTRS trs] of
    [] -> pure trs
    (e : _) -> throwError e
