module Evaluator.Errors where

import Evaluator.Show
import LispParser.Atom

import Text.ParserCombinators.Parsec.Error
--import Control.Monad.Error 

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
showError (TypeMismatch expected found) = "Invalid type: expected "    
    ++ expected ++ ", found " ++ show found 
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm msg form) = msg ++ ": " ++ show form
showError (NotFunction msg func) = msg ++ ": " ++ show func
showError (UnboundVar msg varname) = msg ++ ": " ++ varname
showError (NumArgs expected found) = "Expected" ++ show expected
    ++ " args: found values " ++ unwordsList found 

instance Show LispError where show = showError