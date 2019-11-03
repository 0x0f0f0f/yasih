{-# LANGUAGE ExistentialQuantification #-}
module Yasih.Evaluator.Operators where

import Yasih.LispTypes

import Data.Char (toLower)
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
charBoolBinop = boolBinop unpackChar
ciCharBoolBinop = boolBinop unpackCiChar
strBoolBinop = boolBinop unpackStr
ciStrBoolBinop = boolBinop unpackCiStr
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
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |Unpack strings from LispVal
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- |Unpack strings from LispVal, case insensitive
unpackCiStr :: LispVal -> ThrowsError String
unpackCiStr (String s) = return $ map toLower s
unpackCiStr notString = throwError $ TypeMismatch "string" notString

-- |Unpack characters from LispVal
unpackChar :: LispVal -> ThrowsError Char
unpackChar (Character c) = return c
unpackChar notChar = throwError $ TypeMismatch "char" notChar

unpackCiChar :: LispVal -> ThrowsError Char
unpackCiChar (Character c) = return c
unpackCiChar notChar = throwError $ TypeMismatch "char" notChar

-- |Helper function that takes an Unpacker and determines if two LispVals
-- are equal before unpacking them
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (AnyUnpacker unpacker) = do
    unpacked1 <- unpacker x
    unpacked2 <- unpacker y
    return $ unpacked1 == unpacked2
    `catchError` const (return False)