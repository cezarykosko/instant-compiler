module Main where

  import Grammar.ParGrammar (pProgram)
  import Grammar.AbsGrammar

  import System.Environment (getArgs)

  import Control.Monad.State.Lazy

  import Utils.LLVM
  import Utils.Common

  prefix =  "@dnl = internal constant [4 x i8] c\"%d\\0A\\00\"\n"
          ++ "declare i32 @printf(i8*, ...)\n"
          ++ "define void @printInt(i32 %x) {\n"
          ++ "entry: %t0 = getelementptr [4 x i8]* @dnl, i32 0, i32 0"
          ++ " call i32 (i8*, ...)* @printf(i8* %t0, i32 %x)\n"
          ++ " ret void\n}\n\n"
          ++ "; Function Attrs: nounwind ssp uwtable\n"
          ++ "define i32 @main(i32 %argc, i8** %argv) #0 {\n"
          ++ "  %1 = alloca i32, align 4\n  %2 = alloca i32, align 4\n  %3 = alloca i8**, align 8\n"
          ++ "  store i32 0, i32* %1\n  store i32 %argc, i32* %2, align 4\n"
          ++ "  store i8** %argv, i8*** %3, align 8"
  suffix = "  ret i32 0\n}"

  type Compilation a = StateT Integer IO a
  data Ref =
            Addr String
          | Lit Integer

  instance Show Ref where
    show (Addr s) = "%" ++ s
    show (Lit i) = show i

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
    in do
      putStrLn prefix
      printvar vars
      runStateT (process p) 4
      putStrLn suffix

  process :: Program -> Compilation ()
  process (Prog s) =
    stmtProcess s

  stmtProcess (SExp e : ss) = do
    expRef <- expProcess e
    lift $ putStrLn $ "  call void @printInt(i32 " ++ show expRef ++ ")"
    stmtProcess ss
  stmtProcess (SAss (Ident name) e : ss) = do
    expRef <- expProcess e
    lift $ putStrLn $ "  store i32 " ++ show expRef ++ ", i32* %" ++ name ++ ", align 4"
    stmtProcess ss
  stmtProcess [] = return ()

  getId :: Compilation Ref
  getId = do
    n <- get
    put (n+1)
    return $ Addr (show n)

  expProcess :: Exp -> Compilation Ref
  expProcess (ExpLit i) =
    return $ Lit i
  expProcess (ExpVar (Ident name)) = do
    newID <- getId
    lift $ putStrLn $ "  " ++ show newID ++ " = load i32* %" ++ name ++ ", align 4"
    return newID
  expProcess (ExpAdd e1 e2) = do
    refE1 <- expProcess e1
    refE2 <- expProcess e2
    newID <- getId
    lift $ putStrLn $ "  " ++ show newID ++ " = add nsw i32 " ++ show refE1 ++ ", " ++ show refE2
    return newID
  expProcess (ExpSub e1 e2) = do
    refE1 <- expProcess e1
    refE2 <- expProcess e2
    newID <- getId
    lift $ putStrLn $ "  " ++ show newID ++ " = sub nsw i32 " ++ show refE1 ++ ", " ++ show refE2
    return newID
  expProcess (ExpMul e1 e2) = do
    refE1 <- expProcess e1
    refE2 <- expProcess e2
    newID <- getId
    lift $ putStrLn $ "  " ++ show newID ++ " = mul nsw i32 " ++ show refE1 ++ ", " ++ show refE2
    return newID
  expProcess (ExpDiv e1 e2) = do
    refE1 <- expProcess e1
    refE2 <- expProcess e2
    newID <- getId
    lift $ putStrLn $ "  " ++ show newID ++ " = sdiv i32 " ++ show refE1 ++ ", " ++ show refE2
    return newID
