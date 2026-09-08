-- | 中置式の優先順位解決。
--
--   'HS.Parser' は @a + b * c@ を 'EOpChain' として平坦なまま残す。
--   固定度宣言はモジュール中のどこに書かれていてもよいので、全宣言を
--   読み終えてからでないと木の形が決まらないためである。ここでその
--   'EOpChain' を 'EApply' の木に落とす。
module HS.Fixity
  ( Fixity (..),
    FixityEnv,
    defaultFixities,
    collectFixities,
    resolveModule,
    resolveDecl,
    resolveExpr,
    showFixity,
  )
where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import HS.Error
import HS.Name
import HS.Syntax

data Fixity = Fixity {fixAssoc :: Assoc, fixPrec :: Int}
  deriving (Eq, Show)

type FixityEnv = Map String Fixity

showFixity :: String -> Fixity -> String
showFixity op (Fixity a n) = word a ++ " " ++ show n ++ " " ++ op
  where
    word AssocLeft = "infixl"
    word AssocRight = "infixr"
    word AssocNone = "infix"

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
    go env seen ((op, fx) : rest) = case Map.lookup op seen of
      Just prev
        | prev /= fx -> Left (FixityConflict (showFixity op prev) (showFixity op fx))
      _ -> go (Map.insert op fx env) (Map.insert op fx seen) rest

resolveModule :: FixityEnv -> Module -> Either CompileError Module
resolveModule env = traverse (resolveDecl env)

resolveDecl :: FixityEnv -> Decl -> Either CompileError Decl
resolveDecl env d = case d of
  DFun n cs -> DFun n <$> traverse (resolveClause env) cs
  DData {} -> Right d
  DFixity {} -> Right d

resolveClause :: FixityEnv -> Clause -> Either CompileError Clause
resolveClause env (Clause ps rhs ws) =
  Clause ps <$> resolveRhs env rhs <*> resolveModule env ws

resolveRhs :: FixityEnv -> Rhs -> Either CompileError Rhs
resolveRhs env (Plain e) = Plain <$> resolveExpr env e
resolveRhs env (Guarded gs) =
  Guarded <$> traverse both gs
  where
    both (g, e) = (,) <$> resolveExpr env g <*> resolveExpr env e

resolveAlt :: FixityEnv -> Alt -> Either CompileError Alt
resolveAlt env (Alt p rhs ws) =
  Alt p <$> resolveRhs env rhs <*> resolveModule env ws

resolveExpr :: FixityEnv -> Expr -> Either CompileError Expr
resolveExpr env = go
  where
    go e = case e of
      EOpChain e0 rs -> do
        e0' <- go e0
        rs' <- traverse (\(op, x) -> (,) op <$> go x) rs
        resolveChain env e0' rs'
      EApply f x -> EApply <$> go f <*> go x
      ESectionL op x -> ESectionL op <$> go x
      ESectionR op x -> ESectionR op <$> go x
      EIf c t f -> EIf <$> go c <*> go t <*> go f
      ECase s alts -> ECase <$> go s <*> traverse (resolveAlt env) alts
      ELet ds b -> ELet <$> resolveModule env ds <*> go b
      ELambda ps b -> ELambda ps <$> go b
      EList es -> EList <$> traverse go es
      ETuple es -> ETuple <$> traverse go es
      EVar _ -> Right e
      ECon _ -> Right e
      ELiteral _ -> Right e

resolveChain :: FixityEnv -> Expr -> [(String, Expr)] -> Either CompileError Expr
resolveChain env e0 ops = fst <$> parse ("", Fixity AssocNone (-1)) e0 ops
  where
    parse ::
      (String, Fixity) ->
      Expr ->
      [(String, Expr)] ->
      Either CompileError (Expr, [(String, Expr)])
    parse _ lhs [] = Right (lhs, [])
    parse left@(op1, f1) lhs rest@((op2, rhs) : more)
      | fixPrec f1 == fixPrec f2 && (fixAssoc f1 /= fixAssoc f2 || fixAssoc f1 == AssocNone) =
          Left (FixityConflict (showFixity op1 f1) (showFixity op2 f2))
      | fixPrec f1 > fixPrec f2 || (fixPrec f1 == fixPrec f2 && fixAssoc f1 == AssocLeft) =
          Right (lhs, rest)
      | otherwise = do
          (rhs', more') <- parse (op2, f2) rhs more
          parse left (opApply op2 lhs rhs') more'
      where
        f2 = fixityOf env op2

opApply :: String -> Expr -> Expr -> Expr
opApply op l r = EApply (EApply (opRef op) l) r
  where
    opRef (':' : _) = ECon (ConName op)
    opRef _ = EVar (VarName op)
