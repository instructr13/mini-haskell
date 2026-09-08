module HS.Rename (renameModule) where

import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse (descendExprM)

-- What each source binder has been renamed to. Only binders are in here:
-- a name not present is a top-level name and is left alone.
type Scope = Map.Map VarName VarName

renameModule :: Module -> Compile Module
renameModule = mapM (renameDecl Map.empty)

renameDecl :: Scope -> Decl -> Compile Decl
renameDecl sc d = case d of
  DData {} -> pure d
  DFixity {} -> pure d
  DFun n cs -> DFun n <$> mapM (renameClause sc) cs

renameClause :: Scope -> Clause -> Compile Clause
renameClause sc (Clause ps rhs ws) = do
  (ps', scPats) <- threadPats sc ps
  (ws', sc') <- renameLocals scPats ws
  Clause ps' <$> renameRhs sc' rhs <*> pure ws'

renameAlt :: Scope -> Alt -> Compile Alt
renameAlt sc (Alt p rhs ws) = do
  (p', scPat) <- renamePat sc p
  (ws', sc') <- renameLocals scPat ws
  Alt p' <$> renameRhs sc' rhs <*> pure ws'

renameRhs :: Scope -> Rhs -> Compile Rhs
renameRhs sc rhs = case rhs of
  Plain e -> Plain <$> renameExpr sc e
  Guarded gs -> Guarded <$> mapM both gs
  where
    both (g, e) = (,) <$> renameExpr sc g <*> renameExpr sc e

-- Local definitions are mutually recursive, so their names enter scope
-- before any of their bodies is renamed.
renameLocals :: Scope -> [Decl] -> Compile ([Decl], Scope)
renameLocals sc ds = do
  sc' <- foldM bind sc [n | DFun n _ <- ds]
  ds' <- mapM (renameDecl sc' . renameHead sc') ds
  pure (ds', sc')
  where
    bind s n = flip (Map.insert n) s <$> freshLike n
    renameHead s (DFun n cs) = DFun (Map.findWithDefault n n s) cs
    renameHead _ d = d

renameExpr :: Scope -> Expr -> Compile Expr
renameExpr sc e = case e of
  EVar v -> pure (EVar (Map.findWithDefault v v sc))
  ELet ds b -> do
    (ds', sc') <- renameLocals sc ds
    ELet ds' <$> renameExpr sc' b
  ELambda ps b -> do
    (ps', sc') <- threadPats sc ps
    ELambda ps' <$> renameExpr sc' b
  ECase s alts -> ECase <$> renameExpr sc s <*> mapM (renameAlt sc) alts
  _ -> descendExprM (renameExpr sc) e

-- Renames a list of patterns, each seeing the binders of the ones before it.
threadPats :: Scope -> [Pat] -> Compile ([Pat], Scope)
threadPats = thread renamePat

thread :: (Scope -> a -> Compile (b, Scope)) -> Scope -> [a] -> Compile ([b], Scope)
thread f = go
  where
    go sc [] = pure ([], sc)
    go sc (x : xs) = do
      (y, sc') <- f sc x
      (ys, sc'') <- go sc' xs
      pure (y : ys, sc'')

-- One operand of a pattern operator chain, carrying its operator along.
renameOperand :: Scope -> (String, Pat) -> Compile ((String, Pat), Scope)
renameOperand sc (op, r) = do
  (r', sc') <- renamePat sc r
  pure ((op, r'), sc')

renamePat :: Scope -> Pat -> Compile (Pat, Scope)
renamePat sc q = case q of
  PVar v -> do
    v' <- freshLike v
    pure (PVar v', Map.insert v v' sc)
  PWild -> do
    v <- freshVar "_"
    pure (PVar v, sc)
  PLiteral _ -> pure (q, sc)
  PAs v r -> do
    v' <- freshLike v
    (r', sc') <- renamePat (Map.insert v v' sc) r
    pure (PAs v' r', sc')
  PCon c ps -> rebuild (PCon c) ps
  PList ps -> rebuild PList ps
  PTuple ps -> rebuild PTuple ps
  POpChain p0 rs -> do
    (p0', sc') <- renamePat sc p0
    (rs', sc'') <- thread renameOperand sc' rs
    pure (POpChain p0' rs', sc'')
  where
    rebuild con ps = do
      (ps', sc') <- threadPats sc ps
      pure (con ps', sc')

freshLike :: VarName -> Compile VarName
freshLike (VarName v) = freshVar (nameBase v)
