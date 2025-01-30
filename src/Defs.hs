module Defs where

data SVal = Atom String
  | SList [ SVal ]
  | DottedList [ SVal ] SVal
  | SNumber Integer
  | SString String
  | SBool Bool deriving Show
