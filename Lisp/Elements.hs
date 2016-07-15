module Lisp.Elements where

import Control.Monad
import Data.Char
import Data.Maybe
-- import Data.Either
import Numeric
import Text.ParserCombinators.Parsec hiding (spaces)
import Lisp.LispVal

readBin s = x
  where
    [(x,"")] = Numeric.readInt 2 (`elem` "01") digitToInt s -- probably the best way of doing it; reinventing what's available in the standard libraries isn't a good thing.

spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many ((noneOf "\"")) <|> escaped
  _ <- char '"'
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
  pref <- optionMaybe (char '#' >> (char 'b' <|> char 'o' <|> char 'd' <|> char 'x'))
  sign <- optionMaybe (char '-' <|> char '+')
  number <- many1 digit
  rest <- optionMaybe (char '.' >> many1 digit)
  expo <- optionMaybe ((char 'e' <|> char 'E') >> many1 digit)
  let neg a = case sign of
        Just '-' -> (-a)
        _  -> (a)
  if (isNothing rest && isNothing expo) || (not $ isNothing pref)
    then return $ Number $ neg $ case (fromMaybe 'd' pref) of
      'o' -> fst $ head $ readOct number
      'b' -> readBin number
      'x' -> fst $ head $ readHex number
      _    -> read number
    else return $ Float $ neg $ fst $ head $ readFloat $ (number ++ "." ++ (fromMaybe "0" rest) ++ "e" ++ (fromMaybe "0" expo))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = try parseNumber
            <|> parseAtom
            <|> parseString
            <|> parseQuoted
            <|> do char '('
                   x <- try parseList <|> parseDottedList
                   char ')'
                   return x
