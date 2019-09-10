module Evaluator.Numerical where

import LispTypes
import Environment
import Data.Complex
import Data.Ratio

import Control.Monad.Except

numericalPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericalPrimitives = 
    -- Binary Numerical operations
    [("+", numericBinop (+)), 
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem)]

-- |Unpack numbers from LispValues
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
    if null parsed then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- #TODO extend numericBinop to allow numeric operations on complex
-- numbers and ratios

-- |Accept two numbers and cast one as the appropriate type
numCast :: [LispVal] -> ThrowsError LispVal
-- Same type, just return the two numbers
numCast [a@(Number _),  b@(Number _)]   = return $ List [a,b] 
numCast [a@(Float _),   b@(Float _)]    = return $ List [a,b] 
numCast [a@(Ratio _),   b@(Ratio _)]    = return $ List [a,b]
numCast [a@(Complex _), b@(Complex _)]  = return $ List [a,b]
-- First number is an integer
numCast [(Number a),  b@(Float _)]      = return $ List [Float $ fromInteger a, b] 
numCast [(Number a),  b@(Ratio _)]      = return $ List [Ratio $ fromInteger a, b] 
numCast [(Number a),  b@(Complex _)]    = return $ List [Complex $ fromInteger a, b] 
-- First number is a float
numCast [a@(Float _),  (Number b)]      = return $ List [a, Float $ fromInteger b] 
numCast [a@(Float _),  (Ratio b)]       = return $ List [a, Float $ fromRational b] 
numCast [(Float a),  b@(Complex _)]     = return $ List [Complex $ a :+ 0, b]
-- First number is a rational
numCast [a@(Ratio _),  (Number b)]      = return $ List [a, Ratio $ fromInteger b] 
numCast [(Ratio a),  b@(Float _)]       = return $ List [Float $ fromRational a, b] 
numCast [(Ratio a),  b@(Complex _)]     = return $ List [Complex $ (fromInteger $ numerator a) / (fromInteger $ denominator a), b]
-- First number is a complex
numCast [a@(Complex _),  (Number b)]    = return $ List [a, Complex $ fromInteger b] 
numCast [a@(Complex _),  (Float b)]     = return $ List [a, Complex $ b :+ 0] 
numCast [a@(Complex _),  (Ratio b)]     = return $ List [a, Complex $ (fromInteger $ numerator b) / (fromInteger $ denominator b)]
-- Error cases
numCast [a, b] = case a of
        Number _ -> throwError $ TypeMismatch "number" b
        Float _ -> throwError $ TypeMismatch "number" b
        Ratio _ -> throwError $ TypeMismatch "number" b
        Complex _ -> throwError $ TypeMismatch "number" b
        _ -> throwError $ TypeMismatch "number" a
numCast _ = throwError $ Default "unknown error in numCast"


-- |Take a primitive Haskell Integer function and wrap it
-- with code to unpack an argument list, apply the function to it
-- and return a numeric value
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- Throw an error if there's only one argument
numericBinop op val@[_] = throwError $ NumArgs 2 val
-- Fold the operator leftway if there are enough args
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 