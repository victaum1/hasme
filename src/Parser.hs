module Parser where
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad -- import Monad outdated
import Defs
{-  
 Our strings aren’t quite R5RS compliant, because
 they don’t support escaping of internal quotes
 within the string. Change parseString so that \"
 gives a literal quote character instead of
 terminating the string. You may want to replace none
 Of "\"" with a new parser action that accepts
 either a non-quote character or a backslash followed by a quote mark.
-}
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
parseNumber = liftM (SNumber . read) $ many1 digit

parseExpr :: Parser SVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber

readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> "No match: " ++ show err
  Right _ -> "Found value"
