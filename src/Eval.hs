module Eval where
import Control.Monad.Error (throwError)
import Defs
import Primitives

eval :: SVal-> ThrowsError SVal
eval val@(SString _ ) = return val
eval val@(SNumber _ ) = return val
eval val@(SBool _ ) = return val
eval (SList [Atom "quote", val]) = return val
eval (SList (Atom func : args ) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm ("Unrecognized " ++
  "special form: ") badForm

apply :: String -> [SVal] -> ThrowsError SVal
apply func args = maybe (throwError $ NotAFunc "Unrecognized primitive function args " func ) ($ args) (lookup func
  primitives)
