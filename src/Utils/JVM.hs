module Utils.JVM where

import           Grammar.AbsGrammar

depth :: Program -> [Int]
depth (Prog s) = depth__ s

depth__ :: [Stmt] -> [Int]
depth__ (SAss _ e : ss) = (1 + depth_ e) : depth__ ss
depth__ (SExp e : ss) = depth_ e : depth__ ss
depth__ [] = []

d_ :: Exp -> Exp -> Int
d_ e1 e2 =
  let
    d1 = depth_ e1
    d2 = depth_ e2
  in
    min (max (d1 + 1) d2) (max d1 (d2 + 1))


depth_ :: Exp -> Int
depth_ (ExpAdd e1 e2) = d_ e1 e2
depth_ (ExpSub e1 e2) = d_ e1 e2
depth_ (ExpMul e1 e2) = d_ e1 e2
depth_ (ExpDiv e1 e2) = d_ e1 e2
depth_ (ExpLit i) = 1
depth_ (ExpVar i) = 1
