module Defs where
import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error (Error(noMsg,strMsg), catchError)

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
showVal (SBool False) = "#f"

showVal (SList contents) = "("++ unwordsList contents ++ ")"

showVal (DottedList _head _tail) = "("
  ++ unwordsList _head ++ " . " ++ showVal _tail ++ ")"

unwordsList :: [SVal] -> String
unwordsList = unwords . map show

instance Show SVal where show = showVal

data SValError = NumArgs Integer [SVal]
  | TypeMisMatch String SVal
  | SPError ParseError
  | BadSpecialForm String SVal
  | NotAFunc String String
  | UnboundVar String String
  | DefaultError String

showError :: SValError -> String
showError (UnboundVar message varname) = message ++ ": " ++
  varname
showError (BadSpecialForm message form) = message ++ ": " ++
  show form
showError (NotAFunc message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
  ++ " args; " ++ "Found values: " ++ unwordsList found
showError (TypeMisMatch expected found) = "Invalid type," ++
  " expected: " ++ show expected ++ "; found: " ++ show found
showError (SPError parseError) = "Parse error at: " ++
  show parseError

instance Show SValError where
  show = showError

instance Error SValError where
  noMsg = DefaultError "An error has occurred"
  strMsg = DefaultError

type ThrowsError = Either SValError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
