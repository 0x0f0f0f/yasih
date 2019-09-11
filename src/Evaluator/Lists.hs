module Evaluator.Lists where

import LispTypes
import Environment
import Evaluator.Operators

import Control.Monad.Except

listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrimitives = 
    -- List Primitives
    [("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("append", append),
    ("list", listConstructor),
    -- Type testing functions 
    ("list?", unaryOp listp),
    ("vector?", unaryOp vectorp)]

-- | Type testing functions 
listp, vectorp :: LispVal -> LispVal
listp (List _)          = Bool True
listp (DottedList _ _)  = Bool True
listp _                 = Bool False
vectorp (Vector _)      = Bool True
vectorp _               = Bool False

-- |car returns the head of a list
car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "list" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- |cdr returns the tail of a list
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (_ :xs) x] = return $ DottedList xs x
cdr [DottedList [xs] x] = return x
cdr [badArg] = throwError $ TypeMismatch "list" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- |cons concatenates an element to the head of a list 
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = 
    return $ DottedList (x : xs) xlast 
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

-- #TODO move to stdlib
-- |append concatenates two strings
-- (append '(a) '(b c d))      =>  (a b c d)
append :: [LispVal] -> ThrowsError LispVal
append [List x, List y] = return $ List $ x ++ y
append [DottedList [xs] x, List y] = return $ List $[xs] ++ [x] ++ y
append [List x, DottedList [ys] y] = return $ List $ [ys] ++ [y] ++ x
append [badArg] = throwError $ TypeMismatch "list" badArg
append badArgList = throwError $ NumArgs 2 badArgList

-- #TODO move to stdlib
-- |listConstructor constructs a list from a value
listConstructor :: [LispVal] -> ThrowsError LispVal
listConstructor argList = return $ List argList