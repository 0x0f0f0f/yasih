module Yasih.Evaluator.Vector where

import Yasih.LispTypes
import Yasih.Environment
import Yasih.Evaluator.Operators

import Data.Array
import Control.Monad.Except

vectorPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
vectorPrimitives =
    -- Vector Construction
    [("list->vector", listToVector),
    ("make-vector", makeVector),
    ("vector->list", vectorToList),
    -- Vector Accessors
    ("vector-length", vectorLength),
    ("vector-ref", vectorRef),
    -- Type testing functions
    ("vector?", unaryOp vectorp)]

-- |Type testing functions
vectorp :: LispVal -> LispVal
vectorp (Vector _)  = Bool True
vectorp _           = Bool False

-- |Convert a list to a vector
listToVector :: [LispVal] -> ThrowsError LispVal
listToVector [List x] = return $ Vector $ listArray (0, length x -1) x
listToVector [notList] = throwError $ TypeMismatch "list" notList
listToVector badArgList = throwError $ NumArgs 1 badArgList

-- |Allocate a vector of k elements. If fill is given each element is initialized to fill
makeVector :: [LispVal] -> ThrowsError LispVal
makeVector [Number k] =
    return $ Vector $ listArray (0, fromInteger k - 1) $ replicate (fromInteger k) (List [])
makeVector [Number k, fill] =
    return $ Vector $ listArray (0, fromInteger k - 1) $ replicate (fromInteger k) fill

makeVector (notNum : xs) = throwError $ TypeMismatch "number" notNum
makeVector badArgList = throwError $ NumArgs 1 badArgList

-- |Convert a vector to a list
vectorToList :: [LispVal] -> ThrowsError LispVal
vectorToList [Vector x] = return $ List $ elems x
vectorToList [notVect] = throwError $ TypeMismatch "vector" notVect
vectorToList badArgList = throwError $ NumArgs 1 badArgList

-- |Return the number of elements in vector as an exact integer.
vectorLength :: [LispVal] -> ThrowsError LispVal
vectorLength [Vector x] = return $ Number $ toInteger $ 1 + snd (bounds x)
vectorLength [notVect] = throwError $ TypeMismatch "vector" notVect
vectorLength badArgList = throwError $ NumArgs 1 badArgList

-- |Return the contents of position k of vector. k must be a valid index of vector.
vectorRef :: [LispVal] -> ThrowsError LispVal
vectorRef [Vector x, Number n] =
    let k = fromInteger n in
        if fst (bounds x) <= k && k <= snd (bounds x) then return $ x ! k
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
vectorRef [Vector x, notNum]    = throwError $ TypeMismatch "number" notNum
vectorRef [notVect]             = throwError $ TypeMismatch "vector" notVect
vectorRef badArgList            = throwError $ NumArgs 1 badArgList


{-- |Set the contents at position k of vector. k must be a valid index of vector.
vectorSet :: [LispVal] -> ThrowsError LispVal
vectorSet [Vector x, Number n, obj] =
    let k = fromInteger n in
        if fst (bounds x) <= k && k <= snd (bounds x) then -- #TODO implement vector-set
        else throwError $ Default $ "index " ++ show k ++ " is out of bounds"
vectorSet [Vector x, notNum]    = throwError $ TypeMismatch "number" notNum
vectorSet [Vector x, notNum, _] = throwError $ TypeMismatch "number" notNum
vectorSet [notVect]             = throwError $ TypeMismatch "vector" notVect
vectorSet [notVect, _]          = throwError $ TypeMismatch "vector" notVect
vectorSet [notVect, _, _]       = throwError $ TypeMismatch "vector" notVect
vectorSet badArgList            = throwError $ NumArgs 1 badArgList
--}