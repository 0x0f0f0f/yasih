module LispParser.String where

import LispParser.Atom
import Text.ParserCombinators.Parsec hiding (spaces)

-- Exercise 2.2 and 2.3
-- Parse escaped characters in strings
parseEscapedChars :: Parser Char
parseEscapedChars = do
    char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'
        _ -> x

-- |Parse a string
parseString :: Parser LispVal
parseString = do 
    char '"'
    x <- many (parseEscapedChars <|> noneOf "\"")
    char '"'
    return $ String x