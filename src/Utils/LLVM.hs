module Utils.LLVM where

import qualified Data.Map.Lazy      as Map
import           Grammar.AbsGrammar

lvars :: Program -> [String]
lvars (Prog s) =
  let
    tmp vars (SExp _) = vars
    tmp (num, m) (SAss (Ident name) _) =
      if Map.notMember name m
      then (num + 1, Map.insert name (num + 1) m)
      else (num, m)
    (_, res) = foldl tmp (0, Map.empty) s
  in Map.keys res
