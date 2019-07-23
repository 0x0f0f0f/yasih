module LispParser where

import LispParser.Atom
import LispParser.Bool
import LispParser.Number
import LispParser.String

import Text.ParserCombinators.Parsec hiding (spaces)

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

-- |Parse an expression, now ignoring whitespace
readExpr :: String -> String 
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ (show val)