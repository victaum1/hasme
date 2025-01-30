module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
-- import Control.Monad -- import Monad outdated
import Defs

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~#"

spaces :: Parser ()
spaces = skipMany1 space


parseAtom :: Parser SVal
parseAtom = do first <- (letter <|> symbol)
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                 "#t" -> SBool True
                 "#f" -> SBool False
                 otherwise -> Atom atom

parseString :: Parser SVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ SString x

parseNumber :: Parser SVal
-- parseNumber = liftM (SNumber . read) $ many1 digit
parseNumber = do str <- many1 digit
                 let num = (SNumber . read) str
                 return num

parseExpr :: Parser SVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"
