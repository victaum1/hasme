module Primitives where
import Defs

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
