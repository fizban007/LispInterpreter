module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Char

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] [LispVal]
             | Number Integer
             | String String
             | Bool Bool

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- escaped :: Parser LispVal
-- escaped = do
--   char '\\'
--   char '\"'
--   return $ String "\\\""
readBin s = x
  where
    [(x,"")] = Numeric.readInt 2 (`elem` "01") digitToInt s -- probably the best way of doing it; reinventing what's available in the standard libraries isn't a good thing.

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many ((noneOf "\"")) <|> escaped
  char '"'
  return $ String x
  where escaped = string "\\\""
                  <|> string "\\n"
                  <|> string "\\r"
                  <|> string "\\\\"

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: Parser LispVal
-- parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do
  prefix <- string "#b" <|> string "#o" <|> string "#d" <|> string "#x" <|> string ""
  number <- many1 digit
  return $ Number $ case prefix of
    "#b" -> readBin number
    "#o" -> fst $ head $ readOct number
    "#x" -> fst $ head $ readHex number
    _    -> read number

parseExpr :: Parser LispVal
parseExpr = parseAtom
            <|> parseString
            <|> parseNumber


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

  
main :: IO ()
main = do
  -- putStrLn ("Input name: ")
  args <- getArgs
  -- arg2 <- head $ head getArgs
  putStrLn ( readExpr (args !! 0) )
