module LispParser.Atom where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Data.Complex
import Data.Array

-- |Lisp Atom data type
data LispVal = Atom String -- Simple Types
    | Number Integer
    | Float Double
    | String String
    | Character Char
    | Bool Bool
    | Ratio Rational -- Composite types
    | Complex (Complex Double)
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Vector (Array Int LispVal)
    deriving Eq

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

