module TRS (module TRS) where

import Data.List (intercalate, isPrefixOf, nub, nubBy)

data Term = V String | F String [Term] deriving (Eq)

type Position = [Int]

type Subst = [(String, Term)]

type Rule = (Term, Term)

type TRS = [Rule]

type Strategy = TRS -> Term -> Maybe Term

instance Show Term where
  show (V x) = x
  show (F f ts) = f ++ (if length ts > 0 then "(" ++ intercalate "," [show t | t <- ts] ++ ")" else "")

-- Rename every variable apart by appending a suffix.
-- renameTerm "'" (F "f" [V "x", V "y"]) = F "f" [V "x'", V "y'"]
renameTerm :: String -> Term -> Term
renameTerm suffix (V x) = V (x ++ suffix)
renameTerm suffix (F f ts) = F f [renameTerm suffix t | t <- ts]

-- D(R) = {root(l) | l -> r ∈ R}
definedSymbols :: TRS -> [String]
definedSymbols trs = nub [f | (F f _, _) <- trs]

-- Pos(t)
-- positions (F "add" []) = [[]]
-- positions (F "add" [V "x"]) = [[], [0]]
--                             = [] : [[0]]
--                             = [] : [0 : p | p <- positions (V "x")]
-- positions (F "add" [V "x", V "y"]) = [[], [0], [1]]
--                                    = [] : [[0]] ++ [[1]]
--                                    = [] : [[0]] ++ [p + 1 : ps' | (p : ps') <- [[], [0]]]
--                                    = [] : [0 : p | p <- positions (V "x")]
--                                      ++ [p + 1 : ps' | (p : ps') <- positions (F "add" [V "y"])]
-- positions (F "add" [V "x", V "y", V "z"]) = [[], [0], [1], [2]]
-- positions (F "add" [F "add" [V "x", V "y"], V "y", V "z"]) = [[], [0], [0, 0], [0, 1], [1], [2]]
positions :: Term -> [Position]
positions (V _) = [[]] -- {ε}
positions (F _ []) = [[]] -- {ε}
positions (F _ ts) = [[]] ++ [i : j | i <- [0 .. length ts - 1], j <- positions (ts !! i)]

-- subTermAt t p = t|_p
-- subTermAt (F "add" [V "x", V "y"]) [] = F "add" [V "x", V "y"]
-- subTermAt (F "add" [V "x", V "y"]) [0] = V "x"
-- subTermAt (F "add" [V "x", V "y"]) [1] = V "y"
-- subTermAt (F "add" [(F "add" [V "x", V "y"]), V "y"]) [0, 0] = V "x"
subTermAt :: Term -> Position -> Term
subTermAt t [] = t
subTermAt (V _) _ = error "subTermAt: cannot use sub-position to a variable"
subTermAt (F _ ts) (p : ps) = subTermAt (ts !! p) ps

-- replace t u p = t[u]_p
-- replace (F "add" [V "x", V "y"]) (F "0" []) [0] = F "add" [(F "0" []), V "y"]
-- replace (F "add" [(F "add" [V "x", V "y"]), V "y"]) (F "0" []) [0, 0]
--   = F "add" [F "add" [F "0" [], V "y"], V "y"]
replace :: Term -> Term -> Position -> Term
replace _ u [] = u
replace (V _) _ _ = error "replace: cannot use sub-position to a variable"
replace (F f ts) u (p : ps) = F f [if p' == p then newT else t | (p', t) <- zip [0 .. length ts - 1] ts]
  where
    newT = replace (ts !! p) u ps

-- Var(t)
variables :: Term -> [String]
variables (V x) = [x]
variables (F _ ts) = nub [x | t <- ts, x <- variables t]

-- substitute t sigma = t sigma
substitute :: Term -> Subst -> Term
substitute (V x) sigma
  | Just t <- lookup x sigma = t
  | otherwise = V x
substitute (F f ts) sigma = F f [substitute t sigma | t <- ts]

-- substitute t (compose sigma tau) = substitute (substitute t sigma) tau
compose :: Subst -> Subst -> Subst
compose [] t = t
compose ((s1, V s2) : ss) ts = case lookup s2 ts of
  Just (V t2) | s1 == t2 -> compose ss ts -- Skip variables that map to the same variable (ex. {x ↦ x})
  Just t2 -> (s1, t2) : compose ss ts
  Nothing -> (s1, V s2) : compose ss ts
compose (s : ss) ts = s : compose ss ts

-- Pattern matching auxiliary function
match' :: Subst -> [(Term, Term)] -> Maybe Subst
match' sigma [] = Just sigma
match' sigma ((F f1 ts1, F f2 ts2) : ts)
  -- Rule II: {f(s_1, ..., s_n) ↦ g(t_1, ..., t_n)} ∪ S ==> ⊥ if f /= g
  | f1 /= f2 = Nothing
  -- Rule I: {f(s_1, ..., s_n) ↦ f(t_1, ..., t_n)} ∪ S ==> {s_1 ↦ t_1, ..., s_n ↦ t_n} ∪ S
  | otherwise = match' sigma (zip ts1 ts2 ++ ts)
-- Rule IV: {x ↦ t} ∪ S ==> ⊥ if x ↦ t' ∈ S with t /= t'
match' sigma ((V x, t) : ts) =
  case lookup x sigma of
    Just t' | t /= t' -> Nothing
    Just _ -> match' sigma ts
    _ -> match' ((x, t) : sigma) ts
-- Includes Rule III: {f(s_1, ..., s_n) ↦ x} ∪ S ==> ⊥
match' _ _ = Nothing

-- match s t = Just sigma, if s sigma = t for some sigma
-- match s t = Nothing, otherwise
-- match (F "add" [V "x", (F "s" [(F "add" [V "y", V "z"])])]) (F "add" [(F "s" [V "y"]), (F "s" [(F "add" [(F "add" [V "x", (F "0" [])]), V "z"])])])
--   = Just [("x", F "s" [V "y"]), ("y", F "add" [V "x", (F "0" [])]), ("z", V "z")]
-- match (F "add" [(F "s" [V "x"]), (F "add" [V "x", V "y"])]) (F "add" [(F "s" [(F "add" [(F "0" []), V "x"])]), (F "add" [(F "add" [(F "0" []), (F "0" [])]), V "x"])])
--   = Nothing
match :: Term -> Term -> Maybe Subst
match s t = match' [] [(s, t)]

-- Unification auxiliary function
unify' :: Subst -> [(Term, Term)] -> Maybe Subst
unify' sigma [] = Just sigma
-- Rule I, II
unify' sigma ((F f1 ts1, F f2 ts2) : ts)
  | f1 /= f2 = Nothing
  | otherwise = unify' sigma (zip ts1 ts2 ++ ts)
unify' sigma ((V x, t) : ts)
  | V y <- t, x == y = unify' sigma ts
  | x `elem` tv = Nothing -- x ∈ Var(t) ==> ⊥
  | otherwise = unify' sigma' ts'
  where
    tv = variables t
    ts' = [(substitute t1 [(x, t)], substitute t2 [(x, t)]) | (t1, t2) <- ts]
    sigma' = (x, t) : [(s', substitute t' [(x, t)]) | (s', t') <- sigma]
-- Replace of Rule III: {f(s_1, ..., s_n) ↦ x} ∪ S ==> {x ↦ f(s_1, ..., s_n)} ∪ S to allow bidirectional matching
unify' sigma ((t@(F _ _), V x) : ts) = unify' sigma ((V x, t) : ts)

-- Unification
-- unify (F "f" [V "x", F "a" []]) (F "f" [F "b" [], V "y"]) = Just [("y",a),("x",b)]
-- unify (V "x") (F "f" [V "x"]) = Nothing
unify :: Term -> Term -> Maybe Subst
unify s t = unify' [] [(s, t)]

findTRSMatch :: TRS -> Term -> Maybe (Rule, Subst)
findTRSMatch [] _ = Nothing
findTRSMatch (rule@(l, _) : trs) t
  | Just subst <- maybeSubst = Just (rule, subst)
  | otherwise = findTRSMatch trs t
  where
    maybeSubst = match l t

-- {t | s ->_R t} = {s[r sigma]_p | ∃ p ∈ Pos(s). ∃ l -> r ∈ R. ∃ sigma which satisfies l sigma = s|_p}
reducts :: TRS -> Term -> [(Position, Term)]
reducts trs s = [(p, substitute r sigma) | p <- positions s, (l, r) <- trs, Just sigma <- [match l (subTermAt s p)]]

-- rewrite R t = Just u, if t ->_R u for some term u
-- rewrite R t = Nothing, otherwise
rewrite :: Strategy
rewrite trs s =
  case reducts trs s of
    [] -> Nothing
    rs -> Just (foldl' (\acc (p, t) -> replace acc t p) s (nubBy encloses rs))
  where
    encloses (p, _) (q, _) = p `isPrefixOf` q

-- nf R t = u if t ->_R ... ->_R u for some normal form u
nfWith :: Strategy -> TRS -> Term -> Term
nfWith f trs t
  | Just u' <- u = nfWith f trs u'
  | Nothing <- u = t
  where
    u = f trs t

nfWithLimit :: Int -> TRS -> Term -> Either Term Term
nfWithLimit 0 _ t = Left t
nfWithLimit limit trs t
  | Just u' <- u = nfWithLimit (limit - 1) trs u'
  | Nothing <- u = Right t
  where
    u = rewrite trs t

nf :: TRS -> Term -> Term
nf trs t = nfWith rewrite trs t

showRule :: Rule -> String
showRule (l, r) = show l ++ " -> " ++ show r

showTRS :: TRS -> String
showTRS trs = unlines [showRule rule | rule <- trs]
