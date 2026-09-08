module HS.Lift (liftModule, ParamOrder (..), paramOrderFor) where

import Data.Graph (SCC (..), stronglyConnComp)
import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse

-- Where the captured variables go in the lifted function's parameter list.
--
-- A Clos constructor captures a *prefix* of the parameter list, so anything
-- captured must occupy leading positions when the function is ever used
-- unsaturated. The doc's w-bar-last order is right only for saturated calls
-- (it is what makes @go#1(xs, n)@ come out), so choose per function.
data ParamOrder = ArgsThenFree | FreeThenArgs
  deriving (Eq, Show)

paramOrderFor :: Int -> [Int] -> ParamOrder
paramOrderFor arity uses
  | all (>= arity) uses = ArgsThenFree
  | otherwise = FreeThenArgs

-- One lifted local: its old name, its new top-level name, where the captured
-- variables go, and what was captured.
data Lifted = Lifted
  { liOld :: VarName,
    liNew :: VarName,
    liArity :: Int,
    liOrder :: ParamOrder,
    liCaptured :: [VarName]
  }

liftModule :: Module -> Compile Module
liftModule m = do
  top <- concat <$> mapM liftDecl m
  drain top
  where
    drain acc = do
      ds <- takeDecls
      if null ds
        then pure acc
        else do
          more <- concat <$> mapM liftDecl ds
          drain (acc ++ more)

liftDecl :: Decl -> Compile [Decl]
liftDecl d = case d of
  DData {} -> pure [d]
  DFixity {} -> pure [d]
  DFun n cs -> do
    cs' <- mapM (liftClause (unVarName n)) cs
    pure [DFun n cs']

liftClause :: String -> Clause -> Compile Clause
liftClause origin (Clause ps rhs ws)
  | not (null ws) = residual "where" "HS.Desugar"
  | otherwise = withOrigin origin $ withScope (concatMap patVars ps) $ do
      rhs' <- liftRhs rhs
      pure (Clause ps rhs' [])

liftRhs :: Rhs -> Compile Rhs
liftRhs (Plain e) = Plain <$> liftExpr e
liftRhs (Guarded _) = residual "Guarded" "HS.Desugar"

liftExpr :: Expr -> Compile Expr
liftExpr e = case e of
  -- Outer lets first: doing the inner ones first would leave an already
  -- rewritten inner body referring to a not-yet-lifted outer local function.
  ELet ds b -> do
    ls <- liftLets ds b
    liftExpr (rewriteCalls ls b)
  ELambda {} -> residual "ELambda" "HS.Desugar"
  ECase s alts -> ECase <$> liftExpr s <*> mapM liftAlt alts
  _ -> descendExprM liftExpr e

liftAlt :: Alt -> Compile Alt
liftAlt (Alt q rhs ws)
  | not (null ws) = residual "where" "HS.Desugar"
  | otherwise = withScope (patVars q) (Alt q <$> liftRhs rhs <*> pure [])

-- Lifts one let's declarations, strongly connected component by strongly
-- connected component in dependency order, so that a later group's captured
-- set already reflects the rewriting of the earlier ones.
liftLets :: [Decl] -> Expr -> Compile [Lifted]
liftLets ds body = go (sccOrder ds) []
  where
    go [] acc = pure acc
    go (grp : rest) acc = do
      ls <- liftGroup (map (rewriteDecl acc) grp) body
      go rest (acc ++ ls)

liftGroup :: [Decl] -> Expr -> Compile [Lifted]
liftGroup grp body = do
  captured <- capturedVars [n | DFun n _ <- grp] grp
  ls <- sequence [plan captured n cs | DFun n cs <- grp]
  sequence_ [emitLifted ls l cs | (l, DFun _ cs) <- zip ls grp]
  pure ls
  where
    plan captured n cs = do
      let arity = funArity cs
      new <- freshFun (unVarName n) (arity + length captured)
      pure
        Lifted
          { liOld = n,
            liNew = VarName new,
            liArity = arity,
            liOrder = paramOrderFor arity (callArities n body),
            liCaptured = captured
          }

emitLifted :: [Lifted] -> Lifted -> [Clause] -> Compile ()
emitLifted ls l cs =
  emitDecl
    ( DFun
        (liNew l)
        [ Clause (paramsOf l ps) (rewriteRhs ls rhs) ws
        | Clause ps rhs ws <- cs
        ]
    )

paramsOf :: Lifted -> [Pat] -> [Pat]
paramsOf l ps = case liOrder l of
  ArgsThenFree -> ps ++ extra
  FreeThenArgs -> extra ++ ps
  where
    extra = map PVar (liCaptured l)

-- A call site cannot be rewritten by substituting the variable alone: the
-- captured arguments have to be inserted into the application spine.
--
-- Top-down, because a bottom-up rewrite would reach the bare head @go@ before
-- the spine @go xs@ and insert the captured arguments in the wrong place.
rewriteCalls :: [Lifted] -> Expr -> Expr
rewriteCalls [] e = e
rewriteCalls ls e0 = go e0
  where
    go e = case e of
      EApply _ _ ->
        let (h, as) = appSpine e
            as' = map go as
         in case h of
              EVar v | Just l <- lookupLifted v -> rebuild l as'
              _ -> foldl EApply (go h) as'
      EVar v | Just l <- lookupLifted v -> rebuild l []
      _ -> descendExpr go e

    lookupLifted v = case [l | l <- ls, liOld l == v] of
      (l : _) -> Just l
      [] -> Nothing
    rebuild l as = foldl EApply (EVar (liNew l)) (spread l as)
    spread l as = case liOrder l of
      FreeThenArgs -> map EVar (liCaptured l) ++ as
      ArgsThenFree ->
        let (fixed, over) = splitAt (liArity l) as
         in fixed ++ map EVar (liCaptured l) ++ over

rewriteDecl :: [Lifted] -> Decl -> Decl
rewriteDecl ls (DFun n cs) = DFun n [Clause ps (rewriteRhs ls rhs) ws | Clause ps rhs ws <- cs]
rewriteDecl _ d = d

rewriteRhs :: [Lifted] -> Rhs -> Rhs
rewriteRhs ls (Plain e) = Plain (rewriteCalls ls e)
rewriteRhs ls (Guarded gs) = Guarded [(rewriteCalls ls g, rewriteCalls ls e) | (g, e) <- gs]

-- How many arguments each use site applies, so that a function used
-- unsaturated gets its captured variables first. Counted over maximal
-- spines only: over every subexpression, a saturated call would also be
-- seen as each of its partial prefixes.
callArities :: VarName -> Expr -> [Int]
callArities n whole = [length as | (EVar m, as) <- spines whole, m == n]

-- Dependency order over the group's own names, mutually recursive members
-- grouped together. stronglyConnComp already yields the components in
-- dependency order, which is exactly what liftLets wants.
sccOrder :: [Decl] -> [[Decl]]
sccOrder ds = map flatten (stronglyConnComp nodes)
  where
    names = [n | DFun n _ <- ds]
    nodes = [(d, n, [m | m <- names, m `elem` freeVars d]) | d@(DFun n _) <- ds]
    flatten (AcyclicSCC d) = [d]
    flatten (CyclicSCC grp) = grp
