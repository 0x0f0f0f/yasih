module LispParser.Atom where

import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Data.Complex
import Data.Array
import Data.IORef

import Evaluator.Errors
import Evaluator.Environment

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
    -- Stores a primitive function
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    -- Generalized function type
    | Func {
        params :: [String], -- Parameters name
        vararg :: Maybe String, -- name of a variable-length argument list
        body :: [LispVal], -- list of expressions
        closure :: Env -- the environment where the function was created
        }
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
    return $ Atom ( first : rest )
