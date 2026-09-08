{-# LANGUAGE BangPatterns #-}

module TRS (module TRS) where

import Control.Applicative ((<|>))
import Data.List (intercalate, nub, sortOn, (!?))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set

data Term = V String | F String [Term] deriving (Eq, Ord)

type Position = [Int]

type Subst = [(String, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

type RuleId = Int

-- Which rule fired. 'Builtin' leaves room for the delta-rules of stage 8,
-- which have no Rule to point at.
data RuleRef
  = ByRule !RuleId !Rule
  | Builtin String
  deriving (Eq, Show)

data Redex = Redex
  { rxPos :: !Position,
    rxRule :: !RuleRef
  }
  deriving (Eq, Show)

data Step = Step
  { stepRedexes :: [Redex],
    stepResult :: !Term
  }
  deriving (Eq, Show)

type Strategy = IndexedTRS -> Term -> Maybe Step

data Outcome = Normal | LimitReached
  deriving (Eq, Show)

data Trace = Trace
  { trSteps :: [Step],
    trTerm :: !Term,
    trOutcome :: !Outcome
  }
  deriving (Eq, Show)

instance Show Term where
  show (V x) = x
  show (F f []) = f
  show (F f ts) = f ++ "(" ++ intercalate "," [show t | t <- ts] ++ ")"

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

-- Emits the VAR section too, so that readTRS (showTRS r) = Right r.
-- Sound only when no variable name is also used as a 0-ary symbol, which the
-- compiler guarantees by giving every variable a '#'-suffixed unique name.
showTRS :: TRS -> String
showTRS trs =
  "(VAR "
    ++ unwords (trsVariables trs)
    ++ ")\n(RULES\n"
    ++ concat ["  " ++ showRule rule ++ "\n" | rule <- trs]
    ++ ")\n"

-- D(R) = {root(l) | l -> r ∈ R}
definedSymbols :: TRS -> Set String
definedSymbols trs = Set.fromList [f | (F f _, _) <- trs]

rootSymbol :: Term -> Maybe String
rootSymbol (F f _) = Just f
rootSymbol (V _) = Nothing

-- Pos(t), in pre-order (a position always precedes the positions below it).
positions :: Term -> [Position]
positions t =
  [] : case t of
    V _ -> []
    F _ ts -> [i : p | (i, u) <- zip [0 ..] ts, p <- positions u]

-- t|_p
subTermAt :: Term -> Position -> Maybe Term
subTermAt t [] = Just t
subTermAt (V _) (_ : _) = Nothing
subTermAt (F _ ts) (i : p) = case ts !? i of
  Just u -> subTermAt u p
  Nothing -> Nothing

-- t[u]_p
replace :: Term -> Term -> Position -> Maybe Term
replace _ u [] = Just u
replace (V _) _ (_ : _) = Nothing
replace (F f ts) u (i : p)
  | i >= 0,
    (pre, c : post) <- splitAt i ts =
      case replace c u p of
        Just c' -> Just (F f (pre ++ c' : post))
        Nothing -> Nothing
  | otherwise = Nothing

-- Var(t), each variable once, in first-occurrence order.
variables :: Term -> [String]
variables = nub . variableOccurrences

variableOccurrences :: Term -> [String]
variableOccurrences (V x) = [x]
variableOccurrences (F _ ts) = concatMap variableOccurrences ts

-- The variables occurring more than once, i.e. what breaks left-linearity.
repeatedVariables :: Term -> [String]
repeatedVariables t =
  [x | (x, n) <- Map.toAscList counts, n > (1 :: Int)]
  where
    counts = Map.fromListWith (+) [(x, 1) | x <- variableOccurrences t]

trsVariables :: TRS -> [String]
trsVariables trs = nub [x | (l, r) <- trs, t <- [l, r], x <- variableOccurrences t]

symbolOccurrences :: Term -> [(String, Int)]
symbolOccurrences (V _) = []
symbolOccurrences (F f ts) = (f, length ts) : concatMap symbolOccurrences ts

anySymbol :: (String -> Bool) -> Term -> Bool
anySymbol _ (V _) = False
anySymbol p (F f ts) = p f || any (anySymbol p) ts

renameVars :: (String -> String) -> Term -> Term
renameVars f (V x) = V (f x)
renameVars f (F g ts) = F g [renameVars f t | t <- ts]

-- Rename every variable apart by appending a suffix.
renameTerm :: String -> Term -> Term
renameTerm suffix = renameVars (++ suffix)

-- t sigma
substitute :: Term -> Subst -> Term
substitute (V x) sigma = fromMaybe (V x) (lookup x sigma)
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- substitute t (compose sigma tau) = substitute (substitute t sigma) tau
compose :: Subst -> Subst -> Subst
compose sigma tau =
  [(x, substitute t tau) | (x, t) <- sigma]
    ++ [b | b@(x, _) <- tau, x `notElem` dom]
  where
    dom = map fst sigma

-- Rule I: {f(s_1..s_n) ~ f(t_1..t_n)} => {s_1 ~ t_1, ..., s_n ~ t_n}
-- Rule II: distinct symbols, or distinct arities, do not decompose
decompose :: Term -> Term -> Maybe [(Term, Term)]
decompose (F f ts) (F g us)
  | f == g, length ts == length us = Just (zip ts us)
decompose _ _ = Nothing

-- match s t = Just sigma, if s sigma = t for some sigma
match :: Term -> Term -> Maybe Subst
match s0 t0 = go [] [(s0, t0)]
  where
    go sigma [] = Just sigma
    go sigma ((V x, t) : rest) = case lookup x sigma of
      Just t' | t /= t' -> Nothing
      Just _ -> go sigma rest
      Nothing -> go ((x, t) : sigma) rest
    go sigma ((s, t) : rest) = case decompose s t of
      Just new -> go sigma (new ++ rest)
      Nothing -> Nothing

-- Syntactic unification, with the occurs check.
unify :: Term -> Term -> Maybe Subst
unify s0 t0 = go [] [(s0, t0)]
  where
    go sigma [] = Just sigma
    go sigma ((V x, V y) : rest) | x == y = go sigma rest
    go sigma ((V x, t) : rest)
      | occurs x t = Nothing
      | otherwise = go (bind sigma) [both (`substitute` [(x, t)]) p | p <- rest]
      where
        bind ss = (x, t) : [(y, substitute u [(x, t)]) | (y, u) <- ss]
    go sigma ((s@(F _ _), V x) : rest) = go sigma ((V x, s) : rest)
    go sigma ((s, t) : rest) = case decompose s t of
      Just new -> go sigma (new ++ rest)
      Nothing -> Nothing

    both f (a, b) = (f a, f b)

occurs :: String -> Term -> Bool
occurs x (V y) = x == y
occurs x (F _ ts) = any (occurs x) ts

-- Are s and t unifiable after renaming t's variables apart from s's?
unifiableApart :: Term -> Term -> Bool
unifiableApart s t = maybe False (const True) (unify s (renameVars rename t))
  where
    avoid = variables s ++ variables t
    primes = 1 + maximum (0 : map trailingPrimes avoid)
    rename x = x ++ replicate primes '\''
    trailingPrimes = length . takeWhile (== '\'') . reverse

-- | 左辺の根記号ごとに規則を分類した表。
--
--   対象項の根が f なら、左辺の根が f でない規則は照合しようがない。
--   分類しておけば候補だけを見て済む。意味は変えず、1ステップあたりの
--   照合回数を |R| から |R_f| に減らすだけの最適化である。
--
--   R の順序を保つ分割なので (D1) 和が R で互いに素、(D2) 各 R_f 内の
--   相対順序は R と一致する。
newtype IndexedTRS = IndexedTRS (Map String [(RuleId, Rule)])

indexTRS :: TRS -> IndexedTRS
indexTRS trs =
  IndexedTRS (Map.fromListWith (flip (++)) [(f, [e]) | e@(_, (F f _, _)) <- indexed])
  where
    indexed = zip [0 ..] trs

-- The rules of the index, in the original order.
indexedRules :: IndexedTRS -> TRS
indexedRules (IndexedTRS m) =
  map snd (sortOn fst (concat (Map.elems m)))

rulesFor :: IndexedTRS -> String -> [(RuleId, Rule)]
rulesFor (IndexedTRS m) f = Map.findWithDefault [] f m

-- The first rule of R_f that matches, together with its substitution.
findMatchI :: IndexedTRS -> Term -> Maybe (RuleId, Rule, Subst)
findMatchI _ (V _) = Nothing
findMatchI idx t@(F f _) =
  listToMaybe
    [ (i, rule, sigma)
    | (i, rule@(l, _)) <- rulesFor idx f,
      Just sigma <- [match l t]
    ]

-- The unindexed version, kept as the reference findMatchI is checked against.
findTRSMatch :: TRS -> Term -> Maybe (RuleId, Rule, Subst)
findTRSMatch trs t =
  listToMaybe
    [ (i, rule, sigma)
    | (i, rule@(l, _)) <- zip [0 ..] trs,
      Just sigma <- [match l t]
    ]

-- | 開始項から到達しうる定義記号の規則だけを残す。
--
--   D(t) を t に現れる定義記号とし、S を D(t) から右辺の定義記号を
--   たどった最小の閉集合とすると、還元中に S の外の定義記号が根に
--   現れることはないので、除いた規則は一度も使われない。
prune :: TRS -> Term -> TRS
prune trs t = [rule | rule@(F f _, _) <- trs, Set.member f reachable]
  where
    defined = definedSymbols trs
    symbolsOf u = Set.fromList [f | (f, _) <- symbolOccurrences u, Set.member f defined]
    close s
      | Set.null new = s
      | otherwise = close (Set.union s new)
      where
        new =
          Set.difference
            (Set.unions [symbolsOf r | (F f _, r) <- trs, Set.member f s])
            s
    reachable = close (symbolsOf t)

-- {t | s ->_R t} = {s[r sigma]_p | ∃ p ∈ Pos(s). ∃ l -> r ∈ R. l sigma = s|_p}
reducts :: TRS -> Term -> [(Position, Term)]
reducts trs s =
  [ (p, substitute r sigma)
  | p <- positions s,
    Just u <- [subTermAt s p],
    (l, r) <- trs,
    Just sigma <- [match l u]
  ]

-- The only place rules are applied. First match wins; for a system that
-- passes checkTRS at most one rule can match at the root, so this is a
-- tie-break for ill-formed input rather than a semantic choice.
rootStep :: IndexedTRS -> Term -> Maybe (RuleRef, Term)
rootStep idx t = do
  (i, rule@(_, r), sigma) <- findMatchI idx t
  pure (ByRule i rule, substitute r sigma)

data Depth = Outer | Inner

leftmostOutermost :: Strategy
leftmostOutermost = leftmostStep Outer

leftmostInnermost :: Strategy
leftmostInnermost = leftmostStep Inner

leftmostStep :: Depth -> Strategy
leftmostStep depth idx = go
  where
    go t = case depth of
      Outer -> here t <|> inside t
      Inner -> inside t <|> here t

    here t = (\(ref, u) -> Step [Redex [] ref] u) <$> rootStep idx t

    inside (V _) = Nothing
    inside (F f ts) = descend f 0 [] ts

    descend _ _ _ [] = Nothing
    descend f i done (u : rest) = case go u of
      Just st -> Just (under f i (reverse done) rest st)
      Nothing -> descend f (i + 1) (u : done) rest

under :: String -> Int -> [Term] -> [Term] -> Step -> Step
under f i pre post (Step rs u) =
  Step [Redex (i : p) ref | Redex p ref <- rs] (F f (pre ++ u : post))

-- One parallel-outermost step: contract every maximal outermost redex at once.
-- Each kept position is i : p for a distinct i, so no position is a prefix of
-- another and the replacements cannot interfere.
parallelOutermost :: Strategy
parallelOutermost idx = go
  where
    go t = case rootStep idx t of
      Just (ref, u) -> Just (Step [Redex [] ref] u)
      Nothing -> case t of
        V _ -> Nothing
        F f ts -> combine f (zip [0 ..] [(u, go u) | u <- ts])

    combine f args
      | null redexes = Nothing
      | otherwise = Just (Step redexes (F f children))
      where
        redexes =
          [ Redex (i : p) ref
          | (i, (_, Just st)) <- args,
            Redex p ref <- stepRedexes st
          ]
        children = [maybe u stepResult st | (_, (u, st)) <- args]

-- Prunes to the rules the start term can reach and builds the root-symbol
-- index, both once rather than once per step. Neither changes the result.
prepared :: TRS -> Term -> IndexedTRS
prepared trs t = indexTRS (prune trs t)

data Normalised = Normalised
  { nrTerm :: !Term,
    nrSteps :: !Int,
    nrOutcome :: !Outcome
  }
  deriving (Eq, Show)

normaliseWith :: Strategy -> Int -> TRS -> Term -> Normalised
normaliseWith strategy limit trs t0 = go 0 t0
  where
    idx = prepared trs t0
    go !n !t = case strategy idx t of
      Nothing -> Normalised t n Normal
      Just st
        | n >= limit -> Normalised t n LimitReached
        | otherwise -> go (n + 1) (stepResult st)

-- | 焦点の左右の兄弟と親の記号。項に穴をあけて持つことで、redex を縮約
--   するたびに根までの背骨を作り直さずに済む。
data Crumb = Crumb !String ![Term] ![Term]

crumbSymbol :: Crumb -> String
crumbSymbol (Crumb f _ _) = f

plug :: Crumb -> Term -> Term
plug (Crumb f done rest) t = F f (reverse done ++ t : rest)

unzipTerm :: [Crumb] -> Term -> Term
unzipTerm path t = foldl (flip plug) t path

normaliseOutermost :: Int -> TRS -> Term -> Normalised
normaliseOutermost limit trs t0 = focus 0 0 [] t0
  where
    idx = prepared trs t0
    defined = definedSymbols trs

    -- n: steps taken. d: how many crumbs on the path carry a defined symbol,
    -- i.e. how many ancestors could possibly have become redexes.
    focus !n !d path t
      | Just (_, u) <- rootStep idx t =
          if n >= limit
            then Normalised (unzipTerm path t) n LimitReached
            else contracted (n + 1) d path u
      | otherwise = case t of
          V _ -> next n d path t
          F f ts -> down n d f [] ts path

    -- After a contraction the ancestors have changed. Constructor-rooted ones
    -- can never be redexes in a constructor system, so with no defined symbol
    -- on the path the focus simply stays where it is.
    contracted !n !d path u
      | d == 0 = focus n d path u
      | otherwise = case climb d path u Nothing of
          Nothing -> focus n d path u
          Just (k, ps, parent) -> focus n k ps parent

    -- Walks up looking for the outermost ancestor that has become a redex,
    -- plugging one level at a time. Stops once the last defined-symbol crumb
    -- is behind it: everything above that is a constructor, and a constructor
    -- can never be a redex root in a constructor system.
    climb 0 _ _ best = best
    climb _ [] _ best = best
    climb !k (c : ps) t best
      | isDefined (crumbSymbol c) =
          let k' = k - 1
              best' = if isRedex parent then Just (k', ps, parent) else best
           in climb k' ps parent best'
      | otherwise = climb k ps parent best
      where
        parent = plug c t

    isRedex t = case rootStep idx t of
      Just _ -> True
      Nothing -> False

    down !n !d f done rest path = case rest of
      [] -> next n d path (F f (reverse done))
      (u : us) -> focus n (d + isDef f) (Crumb f done us : path) u

    next !n !d path t = case path of
      [] -> Normalised t n Normal
      (c@(Crumb f done rest) : ps) -> case rest of
        (r : rs) -> focus n d (Crumb f (t : done) rs : ps) r
        [] -> next n (d - isDef (crumbSymbol c)) ps (plug (Crumb f done []) t)

    isDefined f = Set.member f defined
    isDef f = if isDefined f then 1 else 0 :: Int

traceWith :: Strategy -> Int -> TRS -> Term -> Trace
traceWith strategy limit trs t0 = go 0 [] t0
  where
    idx = prepared trs t0
    go n acc t = case strategy idx t of
      Nothing -> stop Normal
      Just st
        | n >= limit -> stop LimitReached
        | otherwise -> go (n + 1) (st : acc) (stepResult st)
      where
        stop o = Trace (reverse acc) t o

nf :: TRS -> Term -> Term
nf trs = nrTerm . normaliseOutermost maxBound trs

nfWithLimit :: Int -> TRS -> Term -> Either Term Term
nfWithLimit limit trs t = case normaliseOutermost limit trs t of
  Normalised u _ Normal -> Right u
  Normalised u _ LimitReached -> Left u
