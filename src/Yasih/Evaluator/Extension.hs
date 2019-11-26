module Yasih.Evaluator.Extension where

-- Primitives that are not in R5RS

import Yasih.LispTypes
import Yasih.Environment
import Yasih.Evaluator.Operators

import Control.Monad.Except

extensionPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
extensionPrimitives =
    [("dump", unaryOp dump),
    ("gettype", gettype)]


-- | Dump a function codeFunc
dump :: LispVal -> LispVal
dump x = String $ dumpVal x

gettype :: [LispVal] -> ThrowsError LispVal
gettype [x] = case x of
    Atom _          -> return $ Atom "symbol"
    Number _        -> return $ Atom "integer"
    Float _         -> return $ Atom "real"
    Ratio _         -> return $ Atom "rational"
    Complex _       -> return $ Atom "complex"
    String _        -> return $ Atom "string"
    Character _     -> return $ Atom "char"
    Bool _          -> return $ Atom "bool"
    List _          -> return $ Atom "list"
    DottedList _ _ -> return $ Atom "list"
    Vector _        -> return $ Atom "vector"
    PrimitiveFunc _ -> return $ Atom "primitive"
    IOFunc _        -> return $ Atom "ioprimitive"
    Func{}          -> return $ Atom "lambda"
    Port _          -> return $ Atom "port"
    _               -> throwError $ Default "unknown type"
gettype badArgList = throwError $ NumArgs 1 badArgList