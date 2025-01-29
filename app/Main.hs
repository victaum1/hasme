module Main where
import System.Environment
-- hook up the module parser to main (TODO)

main :: IO ()
main = do args <- getArgs
          putStrLn ("Hello, " ++ head args)
