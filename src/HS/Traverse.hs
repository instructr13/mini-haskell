module HS.Traverse
  ( Pass (..),
    idPass,
    mkPass,
    stepExpr,
    stepPat,
    stepDecl,
    stepClause,
    stepRhs,
    stepAlt,
    stepDecls,
    descendExprM,
    descendExpr,
    descendPatM,
    transformExprM,
    transformExpr,
    mapExprsM,
    mapPatsM,
    mapRhssM,
    mapClausesM,
    mapAltsM,
    mapDeclsM,
    mapDeclListsM,
    mapLayerM,
    universeExpr,
    childrenExpr,
    spines,
    moduleExprs,
    moduleSpines,
    FreeVarsOf (..),
    freeVars,
    patVars,
    declaredNames,
    substExpr,
    substVar,
  )
where

import Data.Functor.Identity (Identity (..))
import Data.List (nub)
import qualified Data.Set as Set
import HS.Name
import HS.Syntax

-- The AST layers, bundled so that one traversal can be written once and every
-- pass can override just the layers it cares about.
--
-- 'onDecls' is a layer of its own because a declaration *list* -- the module
-- body, a where block, a let block -- is where clause grouping happens, and
-- that is a list operation an element-wise traversal cannot express.
data Pass f = Pass
  { onExpr :: Expr -> f Expr,
    onPat :: Pat -> f Pat,
    onDecl :: Decl -> f Decl,
    onDecls :: [Decl] -> f [Decl],
    onClause :: Clause -> f Clause,
    onRhs :: Rhs -> f Rhs,
    onAlt :: Alt -> f Alt
  }

-- The only place in the program that enumerates every Expr and Pat
-- constructor. Rebuilds one node, delegating each child to the pass.
stepExpr :: (Applicative f) => Pass f -> Expr -> f Expr
stepExpr p e = case e of
  EVar _ -> pure e
  ECon _ -> pure e
  ELiteral _ -> pure e
  EApply a b -> EApply <$> onExpr p a <*> onExpr p b
  EOpChain e0 rs ->
    EOpChain <$> onExpr p e0 <*> traverse (traverse (onExpr p)) rs
  ESectionL op x -> ESectionL op <$> onExpr p x
  ESectionR op x -> ESectionR op <$> onExpr p x
  EIf a b c -> EIf <$> onExpr p a <*> onExpr p b <*> onExpr p c
  ECase s alts -> ECase <$> onExpr p s <*> traverse (onAlt p) alts
  ELet ds b -> ELet <$> onDecls p ds <*> onExpr p b
  ELambda ps b -> ELambda <$> traverse (onPat p) ps <*> onExpr p b
  EList xs -> EList <$> traverse (onExpr p) xs
  ETuple xs -> ETuple <$> traverse (onExpr p) xs

stepPat :: (Applicative f) => Pass f -> Pat -> f Pat
stepPat p q = case q of
  PVar _ -> pure q
  PWild -> pure q
  PLiteral _ -> pure q
  PCon c ps -> PCon c <$> traverse (onPat p) ps
  PAs v r -> PAs v <$> onPat p r
  PList ps -> PList <$> traverse (onPat p) ps
  PTuple ps -> PTuple <$> traverse (onPat p) ps
  POpChain p0 rs -> POpChain <$> onPat p p0 <*> traverse (traverse (onPat p)) rs

stepDecl :: (Applicative f) => Pass f -> Decl -> f Decl
stepDecl p d = case d of
  DData {} -> pure d
  DFixity {} -> pure d
  DFun n cs -> DFun n <$> traverse (onClause p) cs

stepClause :: (Applicative f) => Pass f -> Clause -> f Clause
stepClause p (Clause ps rhs ws) =
  Clause <$> traverse (onPat p) ps <*> onRhs p rhs <*> onDecls p ws

stepRhs :: (Applicative f) => Pass f -> Rhs -> f Rhs
stepRhs p rhs = case rhs of
  Plain e -> Plain <$> onExpr p e
  Guarded gs -> Guarded <$> traverse both gs
  where
    both (g, e) = (,) <$> onExpr p g <*> onExpr p e

stepAlt :: (Applicative f) => Pass f -> Alt -> f Alt
stepAlt p (Alt q rhs ws) =
  Alt <$> onPat p q <*> onRhs p rhs <*> onDecls p ws

stepDecls :: (Applicative f) => Pass f -> [Decl] -> f [Decl]
stepDecls p = traverse (onDecl p)

structural :: (Applicative f) => Pass f -> Pass f
structural p =
  Pass
    { onExpr = stepExpr p,
      onPat = stepPat p,
      onDecl = stepDecl p,
      onDecls = stepDecls p,
      onClause = stepClause p,
      onRhs = stepRhs p,
      onAlt = stepAlt p
    }

idPass :: (Applicative f) => Pass f
idPass = mkPass (\_ base -> base)

-- Ties the recursive knot. Overriding a field of 'idPass' directly would stop
-- the override firing below the first Decl or Alt boundary; going through
-- mkPass makes that impossible, because the handlers it hands you already
-- recurse via the finished pass.
mkPass :: (Applicative f) => (Pass f -> Pass f -> Pass f) -> Pass f
mkPass f = self
  where
    self = f self (structural self)

-- Applies g at every immediate Expr position, including the ones reached
-- through a Decl, Clause, Rhs or Alt layer (a let's bindings, a case
-- alternative's body, a where block). It does not recurse past those
-- positions: that is what transformExprM is for.
descendExprM :: (Applicative f) => (Expr -> f Expr) -> Expr -> f Expr
descendExprM g = stepExpr (mkPass (\_ base -> base {onExpr = g}))

descendExpr :: (Expr -> Expr) -> Expr -> Expr
descendExpr g = runIdentity . descendExprM (Identity . g)

descendPatM :: (Applicative f) => (Pat -> f Pat) -> Pat -> f Pat
descendPatM g = stepPat (mkPass (\_ base -> base {onPat = g}))

-- Bottom-up rewrite of every subexpression.
transformExprM :: (Monad f) => (Expr -> f Expr) -> Expr -> f Expr
transformExprM g = go
  where
    go e = do
      e' <- descendExprM go e
      g e'

transformExpr :: (Expr -> Expr) -> Expr -> Expr
transformExpr g = runIdentity . transformExprM (Identity . g)

-- | Rewrites one layer of the module, bottom-up.
--
--   A layer is named by how to override it in a 'Pass' and how to step it, so
--   each of the map*M functions below is one line rather than another copy of
--   the mkPass knot.
mapLayerM ::
  (Monad f) =>
  (Pass f -> Module -> f Module) ->
  (Pass f -> (a -> f a) -> Pass f) ->
  (Pass f -> a -> f a) ->
  (a -> f a) ->
  Module ->
  f Module
mapLayerM run override step g =
  run (mkPass overridden)
  where
    overridden self base = override base rewrite
      where
        rewrite x = do
          x' <- step self x
          g x'

mapExprsM :: (Monad f) => (Expr -> f Expr) -> Module -> f Module
mapExprsM = mapLayerM (traverse . onDecl) (\p g -> p {onExpr = g}) stepExpr

mapPatsM :: (Monad f) => (Pat -> f Pat) -> Module -> f Module
mapPatsM = mapLayerM (traverse . onDecl) (\p g -> p {onPat = g}) stepPat

mapRhssM :: (Monad f) => (Rhs -> f Rhs) -> Module -> f Module
mapRhssM = mapLayerM (traverse . onDecl) (\p g -> p {onRhs = g}) stepRhs

mapClausesM :: (Monad f) => (Clause -> f Clause) -> Module -> f Module
mapClausesM = mapLayerM (traverse . onDecl) (\p g -> p {onClause = g}) stepClause

mapAltsM :: (Monad f) => (Alt -> f Alt) -> Module -> f Module
mapAltsM = mapLayerM (traverse . onDecl) (\p g -> p {onAlt = g}) stepAlt

mapDeclsM :: (Monad f) => (Decl -> f Decl) -> Module -> f Module
mapDeclsM = mapLayerM (traverse . onDecl) (\p g -> p {onDecl = g}) stepDecl

-- Rewrites every declaration list in the module, innermost first: the module
-- body, every where block and every let block.
mapDeclListsM :: (Monad f) => ([Decl] -> f [Decl]) -> Module -> f Module
mapDeclListsM = mapLayerM onDecls (\p g -> p {onDecls = g}) stepDecls

universeExpr :: Expr -> [Expr]
universeExpr e = e : concatMap universeExpr (childrenExpr e)

childrenExpr :: Expr -> [Expr]
childrenExpr = collected . descendExprM one
  where
    one x = Collect [x]

-- | The expressions a module holds at each Rhs, without descending into
--   them: 'universeExpr' and 'spines' do that, and doing both would visit
--   every node twice.
moduleExprs :: Module -> [Expr]
moduleExprs = collected . onDecls (mkPass collecting)
  where
    collecting _ base = base {onExpr = one}
    one e = Collect [e]

-- | Every maximal application spine in the module.
moduleSpines :: Module -> [(Expr, [Expr])]
moduleSpines m = [s | e <- moduleExprs m, s <- spines e]

-- | Every maximal application spine in the expression.
--
--   'universeExpr' also yields the partial prefixes of an application
--   (@f@, @f a@, @f a b@ for @f a b c@), so counting arguments over it makes a
--   saturated call look unsaturated. This yields each spine once, at its
--   outermost position.
spines :: Expr -> [(Expr, [Expr])]
spines e = case e of
  EApply _ _ ->
    let (h, as) = appSpine e
     in (h, as) : concatMap spines as ++ under h
  _ -> (e, []) : concatMap spines (childrenExpr e)
  where
    under h = case h of
      EVar _ -> []
      ECon _ -> []
      _ -> spines h

-- Collects expressions instead of rebuilding, so that one traversal serves
-- both rewriting and querying. The type parameter is phantom.
newtype Collect a = Collect {collected :: [Expr]}

instance Functor Collect where
  fmap _ (Collect xs) = Collect xs

instance Applicative Collect where
  pure _ = Collect []
  Collect a <*> Collect b = Collect (a ++ b)

-- Free variables, deduplicated, in first-occurrence order. Purely syntactic:
-- it knows nothing about the signature, so callers filter out top-level names.
class FreeVarsOf a where
  -- | Occurrences not bound by the given set, in textual order.
  fvWith :: Set.Set VarName -> a -> [VarName]

freeVars :: (FreeVarsOf a) => a -> [VarName]
freeVars = nub . fvWith Set.empty

-- Extends the bound set with the names a binding form introduces.
binding :: Set.Set VarName -> [VarName] -> Set.Set VarName
binding bound vs = Set.union bound (Set.fromList vs)

instance FreeVarsOf Expr where
  fvWith bound e = case e of
    EVar v -> [v | not (Set.member v bound)]
    ECon _ -> []
    ELiteral _ -> []
    EApply a b -> fv a ++ fv b
    EOpChain e0 rs -> fv e0 ++ [v | (_, x) <- rs, v <- fv x]
    ESectionL _ x -> fv x
    ESectionR _ x -> fv x
    EIf a b c -> fv a ++ fv b ++ fv c
    ECase s alts -> fv s ++ fvWith bound alts
    ELet ds b -> inner ds ++ fvWith inside b
      where
        inside = binding bound (declaredNames ds)
        inner = fvWith inside
    ELambda ps b -> fvWith (binding bound (concatMap patVars ps)) b
    EList xs -> concatMap fv xs
    ETuple xs -> concatMap fv xs
    where
      fv = fvWith bound

instance FreeVarsOf Rhs where
  fvWith bound rhs = case rhs of
    Plain e -> fvWith bound e
    Guarded gs -> [v | (g, e) <- gs, x <- [g, e], v <- fvWith bound x]

instance FreeVarsOf Clause where
  fvWith bound (Clause ps rhs ws) = fvWith inside ws ++ fvWith inside rhs
    where
      inside = binding bound (concatMap patVars ps ++ declaredNames ws)

instance FreeVarsOf Alt where
  fvWith bound (Alt q rhs ws) = fvWith inside ws ++ fvWith inside rhs
    where
      inside = binding bound (patVars q ++ declaredNames ws)

instance FreeVarsOf Decl where
  fvWith bound d = case d of
    DData {} -> []
    DFixity {} -> []
    DFun _ cs -> fvWith bound cs

instance (FreeVarsOf a) => FreeVarsOf [a] where
  fvWith bound xs = concatMap (fvWith bound) xs

patVars :: Pat -> [VarName]
patVars q = case q of
  PVar v -> [v]
  PWild -> []
  PLiteral _ -> []
  PCon _ ps -> concatMap patVars ps
  PAs v r -> v : patVars r
  PList ps -> concatMap patVars ps
  PTuple ps -> concatMap patVars ps
  POpChain p0 rs -> patVars p0 ++ concatMap (patVars . snd) rs

declaredNames :: [Decl] -> [VarName]
declaredNames ds = [n | DFun n _ <- ds]

-- Capture-unaware first-order substitution. Sound only on alpha-converted
-- input, which every pass after HS.Rename has.
substExpr :: [(VarName, Expr)] -> Expr -> Expr
substExpr sub = transformExpr step
  where
    step (EVar v) | Just e <- lookup v sub = e
    step e = e

substVar :: VarName -> VarName -> Expr -> Expr
substVar from to = substExpr [(from, EVar to)]
