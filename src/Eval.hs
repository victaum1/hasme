module Eval where
import Defs

eval :: SVal-> SVal
eval val@(SString _ ) = val
eval val@(SNumber _ ) = val
eval val@(SBool _ ) = val
eval (SList [Atom "quote", val]) = val
eval (SList (Atom func : args ) ) = apply func $ map eval args

apply :: String -> [SVal] -> SVal
apply func args = maybe (SBool False) ($ args) (lookup func
  primitives)

unpackNum :: SVal -> Integer
unpackNum (SNumber n) = n
unpackNum _ = 0

numericBinOp :: (Integer -> Integer -> Integer) -> [SVal] -> SVal
numericBinOp op params = SNumber $ foldl1 op $ map unpackNum
  params

primitives :: [(String, [SVal] -> SVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("reminder", numericBinOp rem)]
