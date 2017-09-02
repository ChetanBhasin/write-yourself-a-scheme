module Lib where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

-- Defines the basic DataType for Atomic Lisp values
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool

-- Basic parser for parsing spaces
spaces :: Parser ()
spaces = skipMany1 space

-- Basic parser for parsing symbols
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Parser for the String type values
parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (noneOf "\"")
    char '"'
    return $ String x

-- Parser for atomic values
parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

-- Parser for numbers
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

-- List parser
parseList :: Parser LispVal
parseList  = liftM List $ sepBy parseExpr spaces

-- Dotted list parser
parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- Quoted strings parser
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

-- Expressions parser
parseExpr :: Parser LispVal
parseExpr = parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> do char '('
           x <- try parseList <|> parseDottedList
           char ')'
           return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"
