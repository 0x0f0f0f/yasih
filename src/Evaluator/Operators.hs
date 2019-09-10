{-# LANGUAGE ExistentialQuantification #-}
module Evaluator.Operators where 

import LispTypes
import Control.Monad.Except

-- |Apply an unary operator 
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

-- |Apply an operator to two arguments and return a Bool
-- boolBinop unpacker operator arguments
-- unpacker is used to unpack the arguments from LispVals to native types
-- op performs the boolean operation
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do 
        left <- unpacker $ head args
        right <- unpacker $ args !! 1
        -- Op function is used as an infix operator by wrapping it in backticks
        return $ Bool $ left `op` right 

-- | Type specific boolean operators
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

-- |Data type that can hold any function to a LispVal into a native type
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- |Unpack a Bool value from a LispVal
unpackBool :: LispVal -> ThrowsError Bool 
unpackBool (Bool b) = return b 
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- |Unpack numbers from LispValues
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
    if null parsed then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |Unpack strings from LispVal
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s 
unpackStr (Bool s ) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- |Helper function that takes an Unpacker and determines if two LispVals
-- are equal before unpacking them
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool 
unpackEquals x y (AnyUnpacker unpacker) = do 
    unpacked1 <- unpacker x 
    unpacked2 <- unpacker y
    return $ unpacked1 == unpacked2 
    `catchError` const (return False)