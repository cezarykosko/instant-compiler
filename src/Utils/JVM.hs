module Utils.JVM where

import           Grammar.AbsGrammar

depth :: Program -> Int
depth (Prog s) = foldl (\x y -> max x $ stmtDepth y) 0 s

stmtDepth (SAss _ e) = 1 + expDepth e
stmtDepth (SExp e) = expDepth e

expDepth :: Exp -> Int
expDepth (ExpAdd e1 e2) = _expDepth e1 e2
expDepth (ExpSub e1 e2) = _expDepth e1 e2
expDepth (ExpMul e1 e2) = _expDepth e1 e2
expDepth (ExpDiv e1 e2) = _expDepth e1 e2
expDepth (ExpLit i) = 1
expDepth (ExpVar i) = 1

_expDepth :: Exp -> Exp -> Int
_expDepth e1 e2 =
  let
    d1 = expDepth e1
    d2 = expDepth e2
  in
    min (max (d1 + 1) d2) (max d1 (d2 + 1))
