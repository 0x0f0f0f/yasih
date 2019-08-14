module Evaluator.Errors (
    module Control.Monad.Except,
    module Evaluator.Errors
) where

import Evaluator.Show
import LispParser.Atom

import Text.ParserCombinators.Parsec.Error
import Control.Monad.Except

{- 
    Some interpreted languages like PHP silently assign
    default values like #f or 0 as a result when errors occur
    This approach means that errors pass silently throughout the
    programs until they can become problematic and need long
    debugging sessions to get rid of the bug completely. 

    Here, errors are signaled as soon as they happen and
    immediately break out of execution 
-}

data LispError = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal 
    | NotFunction String String
    | UnboundVar String String 
    | Default String

-- Make LispError an instance of Show
showError :: LispError -> String
showError (Default msg) = show msg
showError (TypeMismatch expected found) = "Invalid type: expected "    
    ++ expected ++ ", found " ++ show found 
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (NumArgs expected found) = "Expected" ++ show expected
    ++ " args: found values " ++ unwordsList found 

instance Show LispError where show = showError

-- |Type to represent functions that may throw a LispError or return a value.

-- Either is only partially applied (curried) to LispError so that ThrowsError 
-- can be used with any data type
-- Either is a monad and the "extra information" carried by it is whether or not
-- An error has occured. This is why haskell does not need a separate
-- Control-flow construct to handle exceptions.
-- Either also provides throwError and catchError
type ThrowsError = Either LispError

-- |Convert errors to their string representation and return it
-- trapError action 
-- The result of calling trapError is an Either action which will 
-- always have valid (Right) data. 
trapError action = catchError action (return . show)

-- |Extract a value from a ThrowsError type returned by trapError
-- extractValue (Right val) = val
-- extractValue is purposedly left undefined for Left values
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

