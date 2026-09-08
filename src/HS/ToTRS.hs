module HS.ToTRS (toTRS, compileCase, inlineRelays) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import HS.Error
import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse (patVars)
import TRS

toTRS :: Module -> Compile TRS
toTRS m = do
  mapM_ declOf m
  emitted
  where
    declOf d = case d of
      DData {} -> pure ()
      DFixity {} -> pure ()
      DFun n cs -> withOrigin (unVarName n) (mapM_ (clauseRule n) cs)

clauseRule :: VarName -> Clause -> Compile ()
clauseRule n (Clause ps rhs ws) = do
  unless (null ws) (residual "where" "HS.Desugar")
  body <- case rhs of
    Plain e -> pure e
    Guarded _ -> residual "Guarded" "HS.Desugar"
  args <- mapM headTerm ps
  r <- withScope (concatMap patVars ps) (exprTerm body)
  emitRule (F (unVarName n) args, r)

-- Match leaves every clause head all-variables, but HS.Defunc's apply
-- clauses bypass it and are one-level constructor patterns, which are just
-- as well-formed a left-hand side.
headTerm :: Pat -> Compile Term
headTerm p = case p of
  PVar v -> pure (varTerm v)
  PCon c ps -> do
    args <- mapM binder ps
    applyCon c args
  _ -> residual "a nested pattern" "HS.Match"
  where
    binder (PVar v) = pure (varTerm v)
    binder _ = residual "a nested pattern" "HS.Match"

varTerm :: VarName -> Term
varTerm = V . unVarName

exprTerm :: Expr -> Compile Term
exprTerm e = case e of
  EVar v -> do
    top <- isTopLevel v
    if top then applySym (unVarName v) [] else pure (varTerm v)
  ECon c -> applyCon c []
  ECase s alts -> caseTerm s alts
  EApply _ _ -> case appSpine e of
    (EVar v, as) -> do
      top <- isTopLevel v
      if top
        then do
          args <- mapM exprTerm as
          applySym (unVarName v) args
        else residual "an application headed by a variable" "HS.Defunc"
    (ECon c, as) -> do
      args <- mapM exprTerm as
      applyCon c args
    (ECase s alts, as) -> do
      k <- caseTerm s alts
      case as of
        [] -> pure k
        _ -> residual "an application headed by a case" "HS.Defunc"
    _ -> residual "an application of a non-symbol" "HS.Defunc"
  EOpChain _ _ -> residual "EOpChain" "HS.Fixity"
  ESectionL _ _ -> residual "ESectionL" "HS.Desugar"
  ESectionR _ _ -> residual "ESectionR" "HS.Desugar"
  EIf {} -> residual "EIf" "HS.Desugar"
  ELet _ _ -> residual "ELet" "HS.Lift"
  ELambda _ _ -> residual "ELambda" "HS.Lift"
  ELiteral _ -> residual "ELiteral" "HS.Desugar"
  EList _ -> residual "EList" "HS.Desugar"
  ETuple _ -> residual "ETuple" "HS.Desugar"

-- Splits a one-level case into a fresh symbol plus one rule per branch.
caseTerm :: Expr -> [Alt] -> Compile Term
caseTerm s alts = do
  branches <- mapM branchOf alts
  scrutVar <- pure (scrutineeVar s)
  vbar <- capturedVars (concatMap snd3 branches) (map thd3 branches)
  compileCaseWith scrutVar s branches vbar
  where
    branchOf (Alt q rhs ws) = do
      unless (null ws) (residual "where" "HS.Desugar")
      body <- case rhs of
        Plain b -> pure b
        Guarded _ -> residual "Guarded" "HS.Desugar"
      case q of
        PCon c ps -> do
          vs <- mapM binder ps
          pure (c, vs, body)
        _ -> residual "a non-constructor alternative" "HS.Match"
    binder (PVar v) = pure v
    binder _ = residual "a nested alternative pattern" "HS.Match"
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c

scrutineeVar :: Expr -> Maybe VarName
scrutineeVar (EVar v) = Just v
scrutineeVar _ = Nothing

compileCase :: Expr -> [(ConName, [VarName], Expr)] -> [VarName] -> Compile Term
compileCase = compileCaseWith Nothing

compileCaseWith ::
  Maybe VarName ->
  Expr ->
  [(ConName, [VarName], Expr)] ->
  [VarName] ->
  Compile Term
compileCaseWith scrutVar s branches vbar = do
  base <- asks envOrigin
  k <- freshFun base (1 + length vbar)
  case branches of
    [] -> internal "a case with no alternatives"
    ((c0, _, _) : _) -> do
      cons <- siblingsOf c0
      mapM_ (branchRule k) branches
      mapM_ (failRule k) [(c, n) | (c, n) <- cons, c `notElem` covered]
      scrut <- exprTerm s
      pure (F k (scrut : map varTerm vbar))
  where
    covered = [c | (c, _, _) <- branches]
    branchRule k (c, ys, body) = do
      n <- conArity c
      lhs <- branchLhs k c n ys
      -- The alternative's binders take the scrutinee's place in the scope,
      -- which is what fixes the order of the extra arguments.
      r <- inScope ys (exprTerm body)
      emitRule (lhs, r)
    failRule k (c, n) = do
      zs <- mapM (const (freshVar "z")) [1 .. n]
      lhs <- branchLhs k c n zs
      fl <- knownCon KnFail
      emitRule (lhs, F (unConName fl) [])
    -- Discipline 2: the scrutinee is always argument 1, the captured
    -- variables follow in their deterministic order.
    branchLhs k c n ys = do
      scrut <- saturated (unConName c) n (map varTerm ys)
      pure (F k (scrut : map varTerm vbar))
    inScope ys = case scrutVar of
      Just v -> replaceScope v ys
      Nothing -> withScope ys

conArity :: ConName -> Compile Int
conArity c = do
  info <- lookupSym (unConName c)
  case info of
    Just i | isConSym i -> pure (symArity i)
    _ -> throwError (UnknownConstructor c)

-- Builds an application, checking it is saturated. The two entry points
-- differ only in what "unknown" means for them.
saturated :: String -> Int -> [Term] -> Compile Term
saturated name want args
  | want == length args = pure (F name args)
  | otherwise = throwError (ArityError name want (length args))

applySym :: String -> [Term] -> Compile Term
applySym name args = do
  info <- lookupSym name
  case info of
    Nothing -> throwError (UnknownVariable (VarName name))
    Just i -> saturated name (symArity i) args

applyCon :: ConName -> [Term] -> Compile Term
applyCon c args = do
  n <- conArity c
  saturated (unConName c) n args

-- | 恒等置換の中継規則をインライン化する。
--
--   @f(x1..xn) -> g(x1..xn)@ の形 (引数がそのままの順で並ぶ) で g が
--   他のどこにも現れないとき、g の規則の根を f に付け替えて中継を消す。
--
--   置換が恒等でないものはインライン化してはならない。たとえば
--   @split(w,l,ys,zs) -> split#1(l,w,ys,zs)@ を畳むと左辺で構成子の左に
--   変数が来て left-normal 性が壊れる (規律2 の前提)。
inlineRelays :: TRS -> TRS
inlineRelays = go
  where
    go rs = case [(f, g) | (F f as, F g bs) <- rs, isIdentity as bs, foldable rs f g] of
      [] -> rs
      ((f, g) : _) -> go (fold f g rs)

    -- Only the identity permutation is safe. Folding a permuting relay such
    -- as split(w,l,ys,zs) -> split#1(l,w,ys,zs) would put a variable to the
    -- left of a constructor in the left-hand side, which breaks left-normality.
    isIdentity as bs =
      length as == length bs && all isVarT as && and [a == b | (a, b) <- zip as bs]
    isVarT (V _) = True
    isVarT _ = False

    foldable rs f g =
      f /= g
        && rulesOf rs f == 1
        && rulesOf rs g >= 1
        -- g must not be reachable except through this relay, or folding it
        -- into f would lose the other callers.
        && rhsUses rs g == 1
        && lhsArgUses rs g == 0

    rulesOf rs f = length [() | (F h _, _) <- rs, h == f]
    rhsUses rs g = length [() | (_, r) <- rs, (h, _) <- symbolOccurrences r, h == g]
    lhsArgUses rs g =
      length [() | (F _ as, _) <- rs, a <- as, (h, _) <- symbolOccurrences a, h == g]

    fold f g = concatMap step
      where
        step (F h _, F h' _) | h == f, h' == g = []
        step (F h as, r) | h == g = [(F f as, r)]
        step rule = [rule]
