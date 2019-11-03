module Yasih.Evaluator.Lists where

import Yasih.LispTypes
import Yasih.Environment
import Yasih.Evaluator.Operators

import Control.Monad.Except

listPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
listPrimitives =
    -- List Primitives
    [("car", car),
    ("cdr", cdr),
    ("cons", cons),
    -- List selection
    ("list-ref", listRef),
    ("list-tail", listTail),
    ("list-head", listHead),
    ("last", lastBlock),
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


-- |Return the contents of position k of vector. k must be a valid index of vector.
listRef :: [LispVal] -> ThrowsError LispVal
listRef [List x, Number n] =
    let k = fromInteger n in
        if 0 <= k && k <= length x then return $ x !! k
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listRef [List x, notNum]    = throwError $ TypeMismatch "number" notNum
listRef [notVect]           = throwError $ TypeMismatch "vector" notVect
listRef badArgList          = throwError $ NumArgs 1 badArgList

-- | Return the "tail" of lst beginning with its kth element.
listTail :: [LispVal] -> ThrowsError LispVal
listTail [List lst, Number n] =
    let k = fromInteger n in
        if 0 <= k && k <= length lst then return $ List $ drop k lst
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listTail [List lst, notNum]  = throwError $ TypeMismatch "number" notNum
listTail [notVect]           = throwError $ TypeMismatch "vector" notVect
listTail badArgList          = throwError $ NumArgs 1 badArgList

-- | Return the first k elements of lst .
listHead :: [LispVal] -> ThrowsError LispVal
listHead [List lst, Number n] =
    let k = fromInteger n in
        if 0 <= k && k <= length lst then return $ List $ take k lst
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
listHead [List lst, notNum]  = throwError $ TypeMismatch "number" notNum
listHead [notVect]           = throwError $ TypeMismatch "vector" notVect
listHead badArgList          = throwError $ NumArgs 1 badArgList

-- |A last statement returns the last value of a list
lastBlock :: [LispVal] -> ThrowsError LispVal
lastBlock [x] = return x
lastBlock (x:xs) = lastBlock xs
