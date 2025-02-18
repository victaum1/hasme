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
              ( "string>=?", strBoolBinop (>=)),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv)]

car :: [SVal] -> ThrowsError SVal
car [SList (x : xs)] = return x
car [DottedList (x : xs) _ ] = return x
car [badArg] = throwError $ TypeMisMatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [SVal] -> ThrowsError SVal
cdr [SList (x : xs)] = return $ SList xs
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [DottedList [_] x] = return x
cdr [badArg] = throwError $ TypeMisMatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [SVal] -> ThrowsError SVal
cons [x, SList []] = return $ SList [x]
cons [x, SList xs] = return $ SList $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs)
  xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv :: [SVal] -> ThrowsError SVal
eqv [SBool arg1, SBool arg2] = return $ SBool $ arg1 == arg2
eqv [SNumber arg1, SNumber arg2] = return $ SBool $ arg1 == arg2
eqv [SString arg1, SString arg2] = return $ SBool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ SBool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [SList $ xs ++ [x],
    SList $ ys ++ [y]]
eqv [SList arg1, SList arg2] = return $ SBool $ (length arg1 ==
    length arg2) && all eqvPair (zip arg1 arg2) 
    where eqvPair (x1,x2) = case eqv [x1,x2] of
            Left err -> False
            Right (SBool val) -> val
eqv [_,_] = return $ SBool False
eqv badArgList = throwError $ NumArgs 2 badArgList

