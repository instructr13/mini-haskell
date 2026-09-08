-- | デファンクショナライゼーション。
--
--   一階の TRS では記号の arity が固定なので部分適用が表現できない。
--   すべての適用を @ap(ap(map,f),xs)@ にする符号化は項が深くなり構成子系の
--   性質も崩れるので、部分適用を閉包構成子と @apply@ に変換する。
--
--   λ は 'HS.Lift' がトップレベル関数にしているので、ここで扱うのは
--   「arity 未満の適用」と「変数を頭部とする適用」の2つだけである。
--
--   使わない機能のコストは 0 で、部分適用の無いプログラムでは @apply@ の
--   規則が 1 本も出ない。
module HS.Defunc (defunctionalize, closureTyCon, applyName, closureName) where

import Data.List (nub, sort)
import qualified Data.Map.Strict as Map
import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse

-- All closures share one synthetic type, so that siblingsOf on a Clos gives
-- the whole closure family and checkTRS sees a proper constructor system.
closureTyCon :: TyConName
closureTyCon = TyConName "Closure#"

applyName :: VarName
applyName = VarName "apply#"

-- Clos#f#k captures the first k arguments of f. '#' cannot appear in a source
-- identifier, so these can never collide with a user's constructor.
closureName :: String -> Int -> ConName
closureName f k = ConName ("Clos#" ++ f ++ "#" ++ show k)

defunctionalize :: Module -> Compile Module
defunctionalize m = do
  needed <- closuresOf m
  if null needed
    then pure m
    else do
      mapM_ declareClosure needed
      declareFun (unVarName applyName) 2
      m' <- mapM (rewriteDecl (Map.fromList [(fk, ()) | fk <- needed])) m
      apply <- applyDecl needed
      pure (m' ++ [apply])

-- The closure set is upward closed: recording (f, k) demands Clos#f#j for
-- every j in [k, arity f - 1], or the stepwise saturation path has a hole.
closuresOf :: Module -> Compile [(String, Int)]
closuresOf m = do
  found <- sequence [partialOf v (length as) | (EVar v, as) <- moduleSpines m]
  pure (sort (nub [(f, j) | Just (f, k, n) <- found, j <- [k .. n - 1]]))

-- An application of a defined symbol to fewer arguments than its arity,
-- reported as (symbol, arguments given, arity).
partialOf :: VarName -> Int -> Compile (Maybe (String, Int, Int))
partialOf v k = do
  info <- lookupSym (unVarName v)
  pure $ case info of
    Just i | not (isConSym i), k < symArity i -> Just (unVarName v, k, symArity i)
    _ -> Nothing

declareClosure :: (String, Int) -> Compile ()
declareClosure (f, k) =
  declareIfAbsent (unConName (closureName f k)) (SymInfo k (ConSym closureTyCon))

-- ⟦f e1..ek⟧ = Clos#f#k(..)      for k < arity f
-- ⟦f e1..en⟧ = f(..)             for k = arity f
-- ⟦x e⟧      = apply(⟦x⟧, ⟦e⟧)   for a variable head
rewriteDecl :: Map.Map (String, Int) () -> Decl -> Compile Decl
rewriteDecl known d = case d of
  DFun n cs -> DFun n <$> mapM (rewriteClause known) cs
  _ -> pure d

rewriteClause :: Map.Map (String, Int) () -> Clause -> Compile Clause
rewriteClause known (Clause ps rhs ws) =
  Clause ps <$> rewriteRhs known rhs <*> pure ws

rewriteRhs :: Map.Map (String, Int) () -> Rhs -> Compile Rhs
rewriteRhs known rhs = case rhs of
  Plain e -> Plain <$> rewriteExpr known e
  Guarded gs -> Guarded <$> mapM both gs
  where
    both (g, e) = (,) <$> rewriteExpr known g <*> rewriteExpr known e

-- Top-down over application spines: a bare head must not be rewritten before
-- the spine it heads, or its arguments would be lost.
rewriteExpr :: Map.Map (String, Int) () -> Expr -> Compile Expr
rewriteExpr known = go
  where
    go e = case e of
      EApply _ _ -> do
        let (h, as) = appSpine e
        as' <- mapM go as
        case h of
          EVar v -> headed v as'
          ECon _ -> pure (foldl EApply h as')
          _ -> do
            h' <- go h
            pure (applyChain h' as')
      EVar v -> headed v []
      ECase s alts -> ECase <$> go s <*> mapM alt alts
      _ -> descendExprM go e

    alt (Alt q rhs ws) = Alt q <$> rewriteRhs known rhs <*> pure ws

    headed v as = do
      info <- lookupSym (unVarName v)
      case info of
        -- A variable head is a closure at run time, so every argument goes
        -- through apply.
        Nothing -> pure (applyChain (EVar v) as)
        Just i
          | isConSym i -> pure (foldl EApply (ECon (ConName (unVarName v))) as)
          | n <- symArity i,
            length as < n,
            Map.member (unVarName v, length as) known ->
              pure (conApply (closureName (unVarName v) (length as)) as)
          | n <- symArity i, length as >= n ->
              let (fixed, over) = splitAt n as
               in pure (applyChain (foldl EApply (EVar v) fixed) over)
          | otherwise -> pure (foldl EApply (EVar v) as)

    applyChain = foldl (\f x -> EApply (EApply (EVar applyName) f) x)

    conApply c = foldl EApply (ECon c)

-- apply(Clos#f#k(x1..xk), y) = Clos#f#(k+1)(x1..xk, y)   for k+1 < arity f
-- apply(Clos#f#(n-1)(x1..x(n-1)), y) = f(x1..x(n-1), y)  when it saturates
applyDecl :: [(String, Int)] -> Compile Decl
applyDecl needed = do
  cs <- mapM clauseFor needed
  pure (DFun applyName cs)
  where
    clauseFor (f, k) = do
      xs <- mapM (\i -> freshVar ("x" ++ show i)) [1 .. k]
      y <- freshVar "y"
      n <- arityOf f
      let body
            | k + 1 < n = conApply (closureName f (k + 1)) (map EVar (xs ++ [y]))
            | otherwise = foldl EApply (EVar (VarName f)) (map EVar (xs ++ [y]))
      pure
        ( Clause
            [PCon (closureName f k) (map PVar xs), PVar y]
            (Plain body)
            []
        )
    conApply c = foldl EApply (ECon c)
