module Primitives where
import Control.Monad.Error(throwError)
import Data.Functor
import Defs

unpackNum :: SVal -> ThrowsError Integer
unpackNum (SNumber n) = return n
unpackNum notNum = throwError $ TypeMisMatch "Number" notNum
unpackStr :: SVal -> ThrowsError String
unpackStr (SString s) = return s
unpackStr notStr = throwError $ TypeMisMatch "String" notStr
unpackBool :: SVal -> ThrowsError Bool
unpackBool (SBool b) = return b
unpackBool notBool = throwError $ TypeMisMatch "Boolean" notBool

numericBinOp :: (Integer -> Integer -> Integer) -> [SVal] ->
  ThrowsError SVal
numericBinOp _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinOp op params = mapM unpackNum params Data.Functor.<&>
  (SNumber . foldl1 op)

boolBinop :: (SVal -> ThrowsError a) -> (a -> a -> Bool) -> [SVal] -> ThrowsError SVal
boolBinop unpacker op args = if length args /= 2 then throwError $ NumArgs 2 args
  else do
  left <- unpacker (head args)
  right <- unpacker $ args !! 1
  return $ SBool $ left `op` right

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop =boolBinop unpackBool

primitives :: [(String, [SVal] -> ThrowsError SVal)]
primitives = [("+", numericBinOp (+)),
              ("-", numericBinOp (-)),
              ("*", numericBinOp (*)),
              ("/", numericBinOp div),
              ("mod", numericBinOp mod),
              ("quotient", numericBinOp quot),
              ("reminder", numericBinOp rem),
              ( "=" , numBoolBinop (==) ),
              ( "<" , numBoolBinop (<) ),
              ( ">" , numBoolBinop ( >) ),
              ( "/=" , numBoolBinop (/=) ),
              ( ">=" , numBoolBinop (>=) ),
              ( "<=" , numBoolBinop (<=) ),
              ( "&&" , boolBoolBinop (&&) ),
              ( "||" , boolBoolBinop (||)),
              ( "string=?", strBoolBinop (==)),
              ( "string>?", strBoolBinop (>)),
              ( "string<?", strBoolBinop (<)),
              ( "string<=?", strBoolBinop (<=)),
              ( "string>=?", strBoolBinop (>=))]
