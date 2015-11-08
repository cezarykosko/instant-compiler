module Utils.JVM where

import qualified Data.Map.Lazy      as Map
import           Grammar.AbsGrammar
import           Utils.Common

type Depth = Integer

preprocess :: Program -> (Program, Depth, Vars)
preprocess p =
  let
    vars = lvars p
    dep = depth p
    rearranged = rearrange p
    in (rearranged, dep, vars)

lvars :: Program -> Vars
lvars (Prog s) =
  let
    tmp vars (SExp _) = vars
    tmp (num, m) (SAss identifier _) =
      if Map.notMember name m
      then (num + 1, Map.insert name (num + 1) m)
      else (num, m)
      where name = getString identifier
    (_, res) = foldl tmp (0, Map.empty) s
  in res

depth :: Program -> Integer
depth (Prog s) = foldl (\x y -> max x $ stmtDepth y) 0 s

rearrange :: Program -> Program
rearrange (Prog s) = Prog $ map stmtRearrange s

stmtRearrange :: Stmt -> Stmt
stmtRearrange (SAss ident e) = SAss ident $ expRearrange e
stmtRearrange (SExp e) = SExp $ expRearrange e

expRearrange :: Exp -> Exp
expRearrange (ExpAdd e1 e2) =
  let
    d1 = expDepth e1
    d2 = expDepth e2
    m1 = max d1 (d2 + 1)
    m2 = max (d1 + 1) d2
    ne1 = expRearrange e1
    ne2 = expRearrange e2
  in
    if m1 <= m2
    then ExpAdd ne1 ne2
    else ExpAdd ne2 ne1
expRearrange (ExpSub e1 e2) =
  let
    d1 = expDepth e1
    d2 = expDepth e2
    m1 = max d1 (d2 + 1)
    m2 = max (d1 + 1) d2
    ne1 = expRearrange e1
    ne2 = expRearrange e2
  in
    if m1 <= m2
    then ExpSub ne1 ne2
    else ExpSubRev ne2 ne1
expRearrange (ExpMul e1 e2) =
  let
    d1 = expDepth e1
    d2 = expDepth e2
    m1 = max d1 (d2 + 1)
    m2 = max (d1 + 1) d2
    ne1 = expRearrange e1
    ne2 = expRearrange e2
  in
    if m1 <= m2
    then ExpMul ne1 ne2
    else ExpMul ne2 ne1
expRearrange (ExpDiv e1 e2) =
  let
    d1 = expDepth e1
    d2 = expDepth e2
    m1 = max d1 (d2 + 1)
    m2 = max (d1 + 1) d2
    ne1 = expRearrange e1
    ne2 = expRearrange e2
  in
    if m1 <= m2
    then ExpDiv ne1 ne2
    else ExpDivRev ne2 ne1
expRearrange e = e

stmtDepth (SAss _ e) = expDepth e
stmtDepth (SExp e) = max 2 (expDepth e)

getString :: Ident -> String
getString (Ident name) = name

expDepth :: Exp -> Integer
expDepth (ExpAdd e1 e2) = _expDepth e1 e2
expDepth (ExpSub e1 e2) = _expDepth e1 e2
expDepth (ExpSubRev e1 e2) = _expDepth e1 e2
expDepth (ExpMul e1 e2) = _expDepth e1 e2
expDepth (ExpDiv e1 e2) = _expDepth e1 e2
expDepth (ExpDivRev e1 e2) = _expDepth e1 e2
expDepth e = 1

_expDepth :: Exp -> Exp -> Integer
_expDepth e1 e2 =
  let
    d1 = expDepth e1
    d2 = expDepth e2
    max1 = max (d1 + 1) d2
    max2 = max d1 (d2 + 1)
  in
    min max1 max2

jvmConst :: Integer -> String
jvmConst x | x == -1 = "  iconst_m1"
        | x >= 0 && x <=5 = "  iconst_" ++ show x
        | otherwise = "  bipush " ++ show x


jvmLoad :: Int -> String
jvmLoad x
        | x >= 0 && x <=3 = "  iload_" ++ show x
        | otherwise = "  iload " ++ show x

jvmStore :: Int -> String
jvmStore x
        | x >= 0 && x <=3 = "  istore_" ++ show x
        | otherwise = "  istore " ++ show x
