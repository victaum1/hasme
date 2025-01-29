module Main where
import System.Environment
{{-
Exercise 2 (TODO): Change the program so it performs a simple arithmetic operation on the
two arguments and prints out the result. You can use read to convert a
string to a number, and show to convert a number back into a string. Play
around with different operations.
-}}
main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ head args)
