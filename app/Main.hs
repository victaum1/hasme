module Main where
import System.Environment ( getArgs )
import Defs ( extractValue, trapError )
import Parser ( readExpr )
import Eval ( eval )
import System.IO (hFlush,stdout)

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (
  fmap show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred_ prompt action = do
  result <- prompt
  if pred_ result
    then return ()
    else action result >> until_ pred_ prompt action

runREPL :: IO ()
runREPL = until_ (== ":quit") (readPrompt "Squeme> ") evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runREPL
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes 0 or 1 arguments"
