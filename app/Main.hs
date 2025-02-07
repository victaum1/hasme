module Main where
import Control.Monad
import System.Environment
import Defs
import Parser
import Eval

main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
