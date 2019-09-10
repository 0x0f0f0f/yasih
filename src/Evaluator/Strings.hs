module Evaluator.Strings where

import LispTypes
import Environment
import Evaluator.Operators

import Control.Monad.Except

stringPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
stringPrimitives = 
    [("string", stringConstructor), -- String constructors
    ("string=?", strBoolBinop (==)), -- String Comparison
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("string?", unaryOp stringp), -- String predicates
    ("char?", unaryOp charp)]

-- |Type testing functions
stringp, charp :: LispVal -> LispVal
stringp (String _)      = Bool True
stringp _               = Bool False
charp (Character _)     = Bool True
charp _                 = Bool False

-- |Create a string from a list of characters
stringConstructor :: [LispVal] -> ThrowsError LispVal
stringConstructor [] = return $ String ""
stringConstructor charl = makestr (String "") charl
    -- Append Char by Char to the newly allocated string
    where
        makestr :: LispVal -> [LispVal] -> ThrowsError LispVal
        makestr (String str) (Character x : xs) = makestr (String (str ++ [x])) xs
        makestr str@(String _) [] = return str 
        makestr str (x : xs) = throwError $ TypeMismatch "char" x
