module LispParser.Atom where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Data.Complex
import Data.Array

-- |Lisp Atom data type
data LispVal = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    | Number Integer
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    | String String
    | Character Char
    | Bool Bool

-- |Parser that recognizes one of the symbols allowed in Scheme Ident.
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=?>@^_~"

-- |Parser to ignore whitespace
spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispVal
parseAtom = do 
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ Atom ( [first] ++ rest )

