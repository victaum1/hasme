module Defs where

data SVal = Atom String
  | SList [ SVal ]
  | DottedList [ SVal ] SVal
  | SNumber Integer
  | SString String
  | SBool Bool

showVal :: SVal -> String
showVal (SString contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (SNumber contents) = show contents
showVal (SBool True) = "#t"
showVal (SBool false) = "#f"

showVal (SList contents) = "("
  ++ unwordsList contents ++ ")"

showVal (DottedList _head _tail) = "("
  ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"

unwordsList :: [SVal] -> String
unwordsList = unwords . map show

instance Show SVal where show = showVal

