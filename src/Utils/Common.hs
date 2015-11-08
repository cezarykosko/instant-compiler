module Utils.Common where

  import           System.Exit          (exitFailure, exitSuccess)

  import           Grammar.AbsGrammar
  import           Grammar.ErrM
  import           Grammar.LexGrammar
  import           Grammar.ParGrammar
  import           Grammar.PrintGrammar
  import           Grammar.SkelGrammar

  import qualified Data.Map.Lazy as Map

  type ParseFun a = [Token] -> Err a
  type Vars = Map.Map String Integer

  runFile :: ParseFun Program -> (Program -> IO()) -> FilePath -> IO ()
  runFile p callback f = readFile f >>= run p callback

  run :: ParseFun Program -> (Program -> IO()) -> String -> IO ()
  run p callback s = let ts = myLexer s in case p ts of
             Bad s    -> do putStrLn "\nParse              Failed...\n"
                            putStrLn s
                            exitFailure
             Ok  tree -> do
                            callback tree
                            exitSuccess
