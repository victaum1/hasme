module Main where
import System.Environment
{{-
Exercise 1 (TODO): Change the program so it reads two arguments from the command line,
and prints out a message using both of them. 
-}}

main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ head args)
