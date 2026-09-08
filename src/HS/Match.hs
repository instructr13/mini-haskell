module HS.Match (matchModule, matchC) where

import HS.Monad
import HS.Name
import HS.Syntax
import HS.Traverse

-- A pattern column vector and the body it selects.
type Row = ([Pat], Expr)

matchModule :: Module -> Compile Module
matchModule = mapM matchDecl

matchDecl :: Decl -> Compile Decl
matchDecl d = case d of
  DData {} -> pure d
  DFixity {} -> pure d
  DFun n cs -> withOrigin (unVarName n) $ do
    rows <- mapM rowOf cs
    case rows of
      [] -> pure d
      ((ps, _) : _) -> do
        vs <- mapM columnVar (zip [0 :: Int ..] ps)
        body <- withScope vs (matchC vs rows)
        pure (DFun n [Clause (map PVar vs) (Plain body) []])

rowOf :: Clause -> Compile Row
rowOf (Clause ps rhs ws)
  | not (null ws) = residual "where" "HS.Desugar"
  | otherwise = case rhs of
      Plain e -> pure (ps, e)
      Guarded _ -> residual "Guarded" "HS.Desugar"

-- Seeded from the source pattern where there is a name to borrow, so that
-- :dump trs stays diffable against the source.
columnVar :: (Int, Pat) -> Compile VarName
columnVar (_, PVar (VarName v)) = freshVar (nameBase v)
columnVar (i, _) = freshVar ("u" ++ show i)

-- | 複数節・入れ子パターンの定義を、一段 case の木に変換する。
matchC :: [VarName] -> [Row] -> Compile Expr
matchC _ [] = knownRef KnFail
matchC [] ((_, e) : _) = pure e
matchC (u : us) rows
  | all isVarPat (firstColumn rows) = matchC us (map (bindFirst u) rows)
  | otherwise = do
      cons <- columnConstructors rows
      ECase (EVar u) <$> mapM (altFor us rows) cons

firstColumn :: [Row] -> [Pat]
firstColumn rows = [p | (p : _, _) <- rows]

isVarPat :: Pat -> Bool
isVarPat (PVar _) = True
isVarPat _ = False

-- Variable rule: the column's variable becomes the scrutinee variable.
bindFirst :: VarName -> Row -> Row
bindFirst u (PVar w : ps, e) = (ps, substVar w u e)
bindFirst _ (ps, e) = (drop 1 ps, e)

-- One alternative per constructor of the column's type, whether or not a row
-- mentions it: that is what makes the alternatives exhaustive, and an
-- unmentioned constructor simply gets no rows and so reaches
-- patternMatchFail.
altFor :: [VarName] -> [Row] -> (ConName, Int) -> Compile Alt
altFor us rows (c, n) = do
  ys <- mapM freshVar (binderBases c n rows)
  body <- withScope ys (matchC (ys ++ us) (concatMap (specialise ys) rows))
  pure (Alt (PCon c (map PVar ys)) (Plain body) [])
  where
    -- A constructor row contributes its sub-patterns as the new leading
    -- columns; a variable row is specialised to c, rebuilding what it
    -- destructured. Row order is preserved, so the first matching row still
    -- wins -- Haskell's top-to-bottom semantics, for free.
    specialise ys row = case row of
      (PCon d ps : rest, e)
        | d == c -> [(ps ++ rest, e)]
        | otherwise -> []
      (PVar w : rest, e) ->
        [(map PVar ys ++ rest, substExpr [(w, conApplied c ys)] e)]
      _ -> []

conApplied :: ConName -> [VarName] -> Expr
conApplied c ys = foldl EApply (ECon c) (map EVar ys)

-- Names the alternative's binders after the source sub-patterns where a row
-- has one to lend.
binderBases :: ConName -> Int -> [Row] -> [String]
binderBases c n rows = map baseAt [0 .. n - 1]
  where
    baseAt i = case [v | Just v <- [nameAt i]] of
      (v : _) -> v
      [] -> "w"
    nameAt i = case [p | (PCon d ps : _, _) <- rows, d == c, (j, p) <- zip [0 ..] ps, j == i] of
      (PVar (VarName v) : _) -> Just (nameBase v)
      _ -> Nothing

-- The constructors of the type the first column scrutinises. Every
-- constructor pattern in the column must belong to the same type.
columnConstructors :: [Row] -> Compile [(ConName, Int)]
columnConstructors rows = case columnCons of
  [] -> internal "the constructor rule was reached with no constructor in the column"
  (c : rest) -> do
    ty <- typeOfCon c
    mapM_ (requireType ty) rest
    constructorsOf ty
  where
    columnCons = [c | PCon c _ <- firstColumn rows]
    requireType ty d = do
      ty' <- typeOfCon d
      if ty' == ty
        then pure ()
        else
          internal
            ( "a pattern column mixes "
                ++ unConName d
                ++ " :: "
                ++ unTyConName ty'
                ++ " with "
                ++ unTyConName ty
            )
