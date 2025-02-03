module Main where
import System.Environment
import Parser
import Eval

main :: IO ()
main = getArgs >>=
          putStrLn . show . eval . readExpr . head
