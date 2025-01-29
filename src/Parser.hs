module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

readExpr :: String -> String
readExpr input = case parse symbol "scheme" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"
