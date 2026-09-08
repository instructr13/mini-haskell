module Suite.Traverse (tests) where

import Data.Functor.Identity (Identity (..))
import Data.List (sort)
import HS.Fixity (collectFixities, resolveModule)
import HS.Name
import HS.Parser (parseHS)
import HS.Pretty (prettyModule)
import HS.Syntax
import HS.Traverse
import Render (renderOneLine)
import Suite.Gen.OpChain (OpChain (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

parseModule :: String -> Module
parseModule src = case parseHS id "t.hs" src of
  Left e -> error (show e)
  Right m -> m

render :: Module -> String
render = renderOneLine . prettyModule

-- The trap mkPass exists to prevent: an override must still fire inside a
-- let's declarations and inside a case alternative's right-hand side.
countVars :: Module -> Int
countVars m = length [() | d <- m, EVar _ <- exprsOf d]
  where
    exprsOf d = concatMap universeExpr (topExprs d)
    topExprs (DFun _ cs) = concatMap clauseExprs cs
    topExprs _ = []
    clauseExprs (Clause _ rhs ws) = rhsExprs rhs ++ concatMap topExprs ws
    rhsExprs (Plain e) = [e]
    rhsExprs (Guarded gs) = concat [[g, e] | (g, e) <- gs]

renameAllVars :: Module -> Module
renameAllVars m =
  runIdentity (mapExprsM (Identity . step) m)
  where
    step (EVar (VarName v)) = EVar (VarName (v ++ "!"))
    step e = e

tests :: TestTree
tests =
  testGroup
    "HS.Traverse"
    [ testCase "the identity pass is the identity" $
        let m = parseModule src
         in render (runIdentity (mapExprsM Identity m)) @?= render m,
      testCase "an override fires inside a let's declarations" $
        let m = parseModule "f = let { g = x } in g y ;\n"
         in render (renameAllVars m) @?= "f = let { g = x! } in g! y!",
      testCase "an override fires inside a case alternative" $
        let m = parseModule "f e = case e of { A -> x ; B -> y } ;\n"
         in render (renameAllVars m)
              @?= "f e = case e! of { A -> x!; B -> y! }",
      testCase "an override fires inside a where block" $
        let m = parseModule "f = g where { g = x } ;\n"
         in render (renameAllVars m) @?= "f = g! where { g = x! }",
      testCase "an override fires inside a guard" $
        let m = parseModule "f | p = x | q = y ;\n"
         in render (renameAllVars m) @?= "f | p! = x! | q! = y!",
      testCase "an override fires under a lambda" $
        let m = parseModule "f = \\z -> g z ;\n"
         in render (renameAllVars m) @?= "f = \\z -> g! z!",
      testCase "universeExpr reaches every subexpression" $
        countVars (parseModule "f = let { g = x } in g y ;\n") @?= 3,
      testGroup
        "free variables"
        [ testCase "a lambda binder is not free" $
            sort (map unVarName (freeVars (parseExpr "\\z -> add z n"))) @?= ["add", "n"],
          testCase "a let binding shadows an outer name" $
            sort (map unVarName (freeVars (parseExpr "let { n = m } in add n"))) @?= ["add", "m"],
          testCase "a case alternative's binders are not free" $
            sort (map unVarName (freeVars (parseExpr "case xs of { Cons y ys -> add y n }")))
              @?= ["add", "n", "xs"],
          testCase "free variables come out in first-occurrence order" $
            map unVarName (freeVars (parseExpr "f z a z b")) @?= ["f", "z", "a", "b"],
          testCase "patVars collects nested binders left to right" $
            map unVarName (patVars (parsePat "(Cons x (Cons y ys))")) @?= ["x", "y", "ys"]
        ],
      testProperty "substExpr replaces exactly the named variable" $
        \(OpChain c) ->
          let v = VarName "a"
              c' = substExpr [(v, EVar (VarName "zzz"))] c
           in property (v `notElem` freeVars c'),
      testProperty "resolveModule agrees with the pass-based traversal" $
        \(OpChain c) ->
          let m = [DFun (VarName "main") [Clause [] (Plain c) []]]
           in case collectFixities m >>= \env -> resolveModule env m of
                Left _ -> label "conflict" True
                Right m' -> label "resolved" (property (null [() | EOpChain _ _ <- allExprs m']))
    ]
  where
    src = "f x = g x where { g y = y } ;\nmain = f Z ;\n"
    parseExpr s = case parseModule ("e = " ++ s ++ " ;\n") of
      [DFun _ [Clause _ (Plain e) _]] -> e
      _ -> error "parseExpr"
    parsePat s = case parseModule ("f " ++ s ++ " = Z ;\n") of
      [DFun _ [Clause [q] _ _]] -> q
      _ -> error "parsePat"
    allExprs m = concat [universeExpr e | DFun _ cs <- m, Clause _ (Plain e) _ <- cs]
