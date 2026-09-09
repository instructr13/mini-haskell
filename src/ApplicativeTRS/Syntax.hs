module ApplicativeTRS.Syntax (module ApplicativeTRS.Syntax) where

data SExpr
  = SEIdent String
  | SEApp SExpr SExpr
  deriving (Show)

type AppRule = (SExpr, SExpr)

data AppSectionSet = AppSectionSet {ssVars :: [String], ssRules :: [AppRule]}

-- SEApp (SEApp (f a)) b ==> (f, [a,b])
sSpine :: SExpr -> (String, [SExpr])
sSpine = go []
  where
    go acc (SEApp f x) = go (x : acc) f
    go acc (SEIdent n) = (n, acc)
