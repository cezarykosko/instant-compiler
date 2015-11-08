module Main where

  import Grammar.AbsGrammar
  import Grammar.ParGrammar (pProgram)

  import System.Environment (getArgs)

  import Control.Monad.Trans (lift)
  import Control.Monad.Trans.Reader

  import Utils.JVM
  import Utils.Common

  import qualified Data.Map.Lazy as Map

  prefix = "\n.super java/lang/Object\n\n.method <init>()V\n"
          ++ "  .limit stack 1\n  .limit locals 1\n  .line 1\n  aload_0\n"
          ++ "  invokespecial java/lang/Object/<init>()V\n  return\n"
          ++ ".end method\n\n.method public static main([Ljava/lang/String;)V\n"
  suffix = "  return\n.end method"

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      fs -> mapM_ (runFile pProgram callback) fs

  type Compilation a = ReaderT Vars IO a

  callback :: Program -> IO ()
  callback x =
    let
      (program, depth, lvars) = preprocess x
    in do
      putStrLn prefix
      putStrLn $ "  .limit stack " ++ show (1 + depth)
      putStrLn $ "  .limit locals " ++ show ((Map.size lvars) + 1)
      runReaderT (process program) lvars
      putStrLn suffix

  process :: Program -> Compilation ()
  process (Prog s) = stmtProcess s

  stmtProcess :: [Stmt] -> Compilation ()
  stmtProcess [] = return ()
  stmtProcess (SExp e : ss) = do
    expProcess e
    lift $ putStrLn "  getstatic java/lang/System/out Ljava/io/PrintStream;"
    lift $ putStrLn "  swap"
    lift $ putStrLn "  invokevirtual java/io/PrintStream/println(I)V"
    stmtProcess ss
  stmtProcess (SAss (Ident name) e : ss) = do
    expProcess e
    num <- asks (Map.! name)
    lift $ putStrLn $ jvmStore num
    stmtProcess ss

  expProcess :: Exp -> Compilation ()
  expProcess (ExpLit i) =
    lift $ putStrLn $ jvmConst i
  expProcess (ExpVar (Ident name)) = do
    i <- asks (Map.! name)
    lift $ putStrLn $ jvmLoad i
  expProcess (ExpAdd e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  iadd"
  expProcess (ExpMul e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  imul"
  expProcess (ExpSub e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  isub"
  expProcess (ExpSubRev e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  swap"
    lift $ putStrLn "  isub"
  expProcess (ExpDiv e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  idiv"
  expProcess (ExpDivRev e1 e2) = do
    expProcess e1
    expProcess e2
    lift $ putStrLn "  swap"
    lift $ putStrLn "  idiv"
