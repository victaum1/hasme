module Primitives where
import Control.Monad.Error(throwError)
import Data.Functor
import Defs

unpackNum :: SVal -> ThrowsError Integer
unpackNum (SNumber n) = return n
unpackNum notNum = throwError $ TypeMisMatch "Number" notNum

numericBinOp :: (Integer -> Integer -> Integer) -> [SVal] ->
  ThrowsError SVal
numericBinOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params Data.Functor.<&>
  (SNumber . foldl1 op)
--  mapM unpackNum params >>=
--  return . SNumber . foldl1 op

primitives :: [(String, [SVal] -> ThrowsError SVal)]
primitives = [("+", numericBinOp (+)),
             ("-", numericBinOp (-)),
             ("*", numericBinOp (*)),
           ("/", numericBinOp div),
           ("mod", numericBinOp mod),
           ("quotient", numericBinOp quot),
           ("reminder", numericBinOp rem)]
