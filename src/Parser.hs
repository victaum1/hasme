module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
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
                 _ -> Atom atom

parseString :: Parser SVal
parseString = do _ <- char '"'
                 x <- many (noneOf "\"")
                 _ <- char '"'
                 return $ SString x

parseNumber :: Parser SVal
parseNumber = do str <- many1 digit
                 let num = (SNumber . read) str
                 return num

parseQuoted :: Parser SVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ SList [Atom "quote", x]

parseSList :: Parser SVal
parseSList = do ls <- sepBy parseExpr spaces
                let res = SList ls
                return res

parseDList  :: Parser SVal
parseDList  = do
  _head <- endBy parseExpr spaces
  _tail <- char '.' >> spaces >> parseExpr
  return $ DottedList _head _tail

parseList :: Parser SVal
parseList = do
  _ <- char '('
  x <- (try parseSList) <|> parseDList
  _ <- char ')'
  return x


parseExpr :: Parser SVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> parseList

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found : " ++ show val

