module LispParser.Bool where

import LispParser.Atom
import Text.ParserCombinators.Parsec hiding (spaces)

-- |Parse a boolean value
parseBool :: Parser LispVal
parseBool = do
    try $ char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))