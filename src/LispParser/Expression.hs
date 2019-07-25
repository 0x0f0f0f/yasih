module LispParser.Expression where

import LispParser.Atom
import LispParser.Bool
import LispParser.Number
import LispParser.String

import Data.Array

import Text.ParserCombinators.Parsec hiding (spaces)

parseLisp = parse parseExpr "lisp"

-- |Parse an Expression (Either a String, a number or an Atom)
parseExpr :: Parser LispVal 
parseExpr = parseAtom
    <|> parseString
    <|> try parseRatio
    <|> try parseComplex
    <|> try parseFloat
    <|> try parseNumber
    <|> try parseBool
    <|> try parseCharacter
    <|> try parseQuoted
    <|> try parseQuasiQuoted
    <|> try parseUnQuote
    <|> try parseVector
    <|> try parseParens

-- |Parse a List of Atoms like a b c d
parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= (return . List)

-- |Parse a Dotted list (a b c . d)
parseDottedList :: Parser LispVal
parseDottedList = do
    -- Parse a List of 0 or more expressions 
    -- Separated by spaces
    head <- endBy parseExpr spaces  
    -- Then parse the remaining Expr after the dot
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

-- |Parse a Quoted Expression 'a
parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]


-- |Parse a QuasiQuoted Expression
-- See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.6
parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char '`'
    x <- parseExpr
    return $ List [Atom "unquote", x]
    

-- |Parse a list expression surrounded by parens 
parseParens :: Parser LispVal
parseParens = do
    char '('
    x <- (try parseList) <|> parseDottedList
    char ')'
    return x

-- |Parse a Vector #(a b c)
-- |See https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-9.html#%_sec_6.3.6
parseVector :: Parser LispVal
parseVector = do
    string "#("
    vals <- sepBy parseExpr spaces
    char ')'
    return $ 
        Vector $ listArray (0, (length vals -1)) vals