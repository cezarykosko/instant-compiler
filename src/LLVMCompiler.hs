module Main where

  import Grammar.ParGrammar (pProgram)
  import Grammar.AbsGrammar

  import System.Environment (getArgs)

  import Utils.LLVM
  import Utils.Common

  prefix = ""
  suffix = ""
  
  main :: IO ()
  main = do
    args <- getArgs
    case args of
      [] -> getContents >>= run pProgram callback
      fs -> mapM_ (runFile pProgram callback) fs

  callback :: Program -> IO ()
  callback p = let
    vars = lvars p
    printvar (var:varss) = do
      putStrLn $ "  %" ++ var ++ " = alloca i32, align 4"
      printvar varss
    printvar [] = return ()
    in
      printvar vars
