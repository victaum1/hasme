module Eval where
import Defs
import Primitives

eval :: SVal-> SVal
eval val@(SString _ ) = val
eval val@(SNumber _ ) = val
eval val@(SBool _ ) = val
eval (SList [Atom "quote", val]) = val
eval (SList (Atom func : args ) ) = apply func $ map eval args

apply :: String -> [SVal] -> SVal
apply func args = maybe (SBool False) ($ args) (lookup func
  primitives)
