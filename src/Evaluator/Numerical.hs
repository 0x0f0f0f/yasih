{-# LANGUAGE FlexibleContexts #-}

module Evaluator.Numerical where

import LispTypes
import Environment
import Evaluator.Operators

import Data.Complex
import Data.Ratio
import Data.Foldable
import Data.Fixed 
import Numeric
import Control.Monad.Except

numericalPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
numericalPrimitives = 
    -- Binary Numerical operations
    [("+", numAdd), 
    ("-", numSub),
    ("*", numMul),
    ("/", numDivide),
    ("modulo", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    -- Numerical Boolean operators
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    -- Type testing functions
    ("number?", unaryOp numberp),
    ("integer?", unaryOp integerp),
    ("float?", unaryOp floatp),
    ("ratio?", unaryOp ratiop),
    ("complex?", unaryOp complexp)]

-- |Type testing functions
numberp, integerp, floatp, ratiop, complexp :: LispVal -> LispVal
integerp (Number _)     = Bool True
integerp _              = Bool False
numberp (Number _)      = Bool True
numberp (Float _)       = Bool True
numberp (Ratio _)       = Bool True
numberp (Complex _)     = Bool True
numberp _               = Bool False
floatp (Float _)        = Bool True
floatp _                = Bool False
ratiop (Ratio _)        = Bool True
ratiop _                = Bool False
complexp (Complex _)    = Bool True
complexp _              = Bool False

-- |foldl1M is like foldlM but has no base case
foldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a 
foldl1M f (x : xs) = foldlM f x xs
foldl1M _ _ = error "unexpected error in foldl1M"

-- | Sum numbers
numAdd :: [LispVal] -> ThrowsError LispVal
numAdd []   = return $ Number 0
numAdd l    = foldl1M (\ x y -> numCast [x, y] >>= go) l
    where 
        go (List [Number x, Number y])      = return $ Number $ x + y 
        go (List [Float x, Float y])        = return $ Float $ x + y
        go (List [Ratio x, Ratio y])        = return $ Ratio $ x + y
        go (List [Complex x, Complex y])    = return $ Complex $ x + y
        go _ = throwError $ Default "unexpected error in (+)"

        -- | Subtract numbers
numSub :: [LispVal] -> ThrowsError LispVal
numSub []           = throwError $ NumArgs 1 []
numSub [Number x]   = return $ Number $ -1 * x
numSub [Float x]    = return $ Float $ -1 * x
numSub [Ratio x]    = return $ Ratio $ -1 * x 
numSub [Complex x]  = return $ Complex $ -1 * x
numSub l = foldl1M (\ x y -> numCast [x, y] >>= go) l
    where 
        go (List [Number x, Number y])      = return $ Number $ x - y 
        go (List [Float x, Float y])        = return $ Float $ x - y
        go (List [Ratio x, Ratio y])        = return $ Ratio $ x - y
        go (List [Complex x, Complex y])    = return $ Complex $ x - y
        go _ = throwError $ Default "unexpected error in (-)"

-- | Multiply numbers
numMul :: [LispVal] -> ThrowsError LispVal
numMul []   = return $ Number 1
numMul l    = foldl1M (\ x y -> numCast [x, y] >>= go) l
    where 
        go (List [Number x, Number y])      = return $ Number $ x * y 
        go (List [Float x, Float y])        = return $ Float $ x * y
        go (List [Ratio x, Ratio y])        = return $ Ratio $ x * y
        go (List [Complex x, Complex y])    = return $ Complex $ x * y
        go _ = throwError $ Default "unexpected error in (*)"

-- |Divide two numbers
numDivide :: [LispVal] -> ThrowsError LispVal
numDivide [] = throwError $ NumArgs 1 []
numDivide [Number 0]   = throwError DivideByZero
numDivide [Ratio 0]     = throwError DivideByZero
numDivide [Number x]   = return $ Ratio $ 1 / fromInteger x
numDivide [Float x]    = return $ Float $ 1.0 / x
numDivide [Ratio x]    = return $ Ratio $ 1 / x 
numDivide [Complex x]  = return $ Complex $ 1 / x
numDivide l = foldl1M (\ x y -> numCast [x, y] >>= go) l
    where 
        go (List [Number x, Number y])
            | y == 0 = throwError DivideByZero
            | mod x y == 0 = return $ Number $ div x y -- Integer division
            | otherwise = return $ Ratio $ fromInteger x / fromInteger y   
        go (List [Float x, Float y])
            | y == 0 = throwError DivideByZero
            | otherwise = return $ Float $ x / y
        go (List [Ratio x, Ratio y])
            | y == 0 = throwError DivideByZero
            | otherwise = return $ Ratio $ x / y
        go (List [Complex x, Complex y])
            | y == 0 = throwError DivideByZero
            | otherwise = return $ Complex $ x / y
        go _ = throwError $ Default "unexpected error in (/)"

-- |Numerical modulus
numMod :: [LispVal] -> ThrowsError LispVal
numMod [] = return $ Number 1
numMod l = foldl1M (\ x y -> numCast [x, y] >>= go) l
    where
        go (List [Number a, Number b]) = return $ Number $ mod' a b
        go (List [Float a, Float b]) = return $ Float $ mod' a b
        go (List [Ratio a, Ratio b]) = return $ Ratio $ mod' a b
        -- #TODO implement modulus for complex numbers
        go (List [Complex a, Complex b]) = throwError $ Default "modulus is not yet implemented for complex numbers"
        go _ = throwError $ Default "unexpected error in (modulus)"

-- |Accept two numbers and cast one as the appropriate type
numCast :: [LispVal] -> ThrowsError LispVal
-- Same type, just return the two numbers
numCast [a@(Number _),  b@(Number _)]   = return $ List [a,b] 
numCast [a@(Float _),   b@(Float _)]    = return $ List [a,b] 
numCast [a@(Ratio _),   b@(Ratio _)]    = return $ List [a,b]
numCast [a@(Complex _), b@(Complex _)]  = return $ List [a,b]
-- First number is an integer
numCast [Number a,  b@(Float _)]      = return $ List [Float $ fromInteger a, b] 
numCast [Number a,  b@(Ratio _)]      = return $ List [Ratio $ fromInteger a, b] 
numCast [Number a,  b@(Complex _)]    = return $ List [Complex $ fromInteger a, b] 
-- First number is a float
numCast [a@(Float _),  Number b]      = return $ List [a, Float $ fromInteger b] 
numCast [a@(Float _),  Ratio b]       = return $ List [a, Float $ fromRational b] 
numCast [Float a,  b@(Complex _)]     = return $ List [Complex $ a :+ 0, b]
-- First number is a rational
numCast [a@(Ratio _),  Number b]      = return $ List [a, Ratio $ fromInteger b] 
numCast [Ratio a,  b@(Float _)]       = return $ List [Float $ fromRational a, b] 
numCast [Ratio a,  b@(Complex _)]     = return $ List [Complex $ fromInteger (numerator a) / fromInteger (denominator a), b]
-- First number is a complex
numCast [a@(Complex _),  Number b]    = return $ List [a, Complex $ fromInteger b] 
numCast [a@(Complex _),  Float b]     = return $ List [a, Complex $ b :+ 0] 
numCast [a@(Complex _),  Ratio b]     = return $ List [a, Complex $ fromInteger (numerator b) / fromInteger (denominator b)]
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