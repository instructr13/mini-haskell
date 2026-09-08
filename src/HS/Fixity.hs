module HS.Fixity
  ( Fixity (..),
    FixityEnv,
    defaultFixities,
    fallbackFixity,
    fixityOf,
    collectFixities,
    resolveModule,
    resolveFixity,
    resolveChain,
    resolvePatChain,
    showFixity,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HS.Error
import HS.Syntax
import HS.Traverse

data Fixity = Fixity {fixAssoc :: Assoc, fixPrec :: Int}
  deriving (Eq, Show)

type FixityEnv = Map String Fixity

showFixity :: String -> Fixity -> String
showFixity op (Fixity a n) = assocKeyword a ++ " " ++ show n ++ " " ++ op

defaultFixities :: FixityEnv
defaultFixities =
  Map.fromList $
    concat
      [ at AssocRight 9 ["."],
        at AssocRight 8 ["^", "^^", "**"],
        at AssocLeft 7 ["*", "/", "quot", "rem", "div", "mod"],
        at AssocLeft 6 ["+", "-"],
        at AssocRight 5 [":", "++"],
        at AssocNone 4 ["==", "/=", "<", "<=", ">=", ">"],
        at AssocRight 3 ["&&"],
        at AssocRight 2 ["||"]
      ]
  where
    at a n ops = [(op, Fixity a n) | op <- ops]

fallbackFixity :: Fixity
fallbackFixity = Fixity AssocLeft 9

fixityOf :: FixityEnv -> String -> Fixity
fixityOf env op = Map.findWithDefault fallbackFixity op env

collectFixities :: Module -> Either CompileError FixityEnv
collectFixities ds = go defaultFixities Map.empty declared
  where
    declared = [(op, Fixity a n) | DFixity a n ops <- ds, op <- ops]
    go env _ [] = Right env
    go env seen ((op, fx) : rest)
      | fixPrec fx < 0 || fixPrec fx > 9 =
          Left (FixityLevelOutOfRange op (fixPrec fx))
      | otherwise = case Map.lookup op seen of
          Just prev
            | prev /= fx ->
                Left (FixityConflict (showFixity op prev) (showFixity op fx))
          _ -> go (Map.insert op fx env) (Map.insert op fx seen) rest

resolveModule :: FixityEnv -> Module -> Either CompileError Module
resolveModule env = traverse (onDecl (fixityPass env))

resolveFixity :: FixityEnv -> Expr -> Either CompileError Expr
resolveFixity env = onExpr (fixityPass env)

-- Everything except EOpChain is the structural traversal, so the pass is
-- exactly the one interesting case.
fixityPass :: FixityEnv -> Pass (Either CompileError)
fixityPass env = mkPass $ \self base ->
  base
    { onExpr = resolvedExpr self base,
      onPat = resolvedPat self base
    }
  where
    resolvedExpr self base e = case e of
      EOpChain _ _ -> do
        e' <- stepExpr self e
        case e' of
          EOpChain e0 rs -> resolveChain env e0 rs
          _ -> Right e'
      _ -> onExpr base e

    resolvedPat self base q = case q of
      POpChain _ _ -> do
        q' <- stepPat self q
        case q' of
          POpChain p0 rs -> resolvePatChain env p0 rs
          _ -> Right q'
      _ -> onPat base q

resolveChain :: FixityEnv -> Expr -> [(String, Expr)] -> Either CompileError Expr
resolveChain env = resolveChainWith opApply env Nothing

resolvePatChain :: FixityEnv -> Pat -> [(String, Pat)] -> Either CompileError Pat
resolvePatChain env = resolveChainWith opPatApply env Nothing

-- Precedence climbing. 'left' is the operator we are currently the
-- right-hand operand of; Nothing at the top, which removes the need for a
-- sentinel fixity that could collide with a real one.
resolveChainWith ::
  (String -> a -> a -> a) ->
  FixityEnv ->
  Maybe (String, Fixity) ->
  a ->
  [(String, a)] ->
  Either CompileError a
resolveChainWith app env left0 lhs0 ops0 = fst <$> parse left0 lhs0 ops0
  where
    parse _ lhs [] = Right (lhs, [])
    parse left lhs rest@((op2, rhs) : more)
      | Just (op1, f1) <- left,
        conflicts f1 f2 =
          Left (FixityConflict (showFixity op1 f1) (showFixity op2 f2))
      | Just (_, f1) <- left, yields f1 f2 = Right (lhs, rest)
      | otherwise = do
          (rhs', more') <- parse (Just (op2, f2)) rhs more
          parse left (app op2 lhs rhs') more'
      where
        f2 = fixityOf env op2

    conflicts f1 f2 =
      fixPrec f1 == fixPrec f2
        && (fixAssoc f1 /= fixAssoc f2 || fixAssoc f1 == AssocNone)

    yields f1 f2 =
      fixPrec f1 > fixPrec f2
        || (fixPrec f1 == fixPrec f2 && fixAssoc f1 == AssocLeft)
