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
    ("modulo", numMod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("abs", numAbs),
    ("ceiling", numCeil),
    ("floor", numFloor),
    ("round", numRound),
    ("truncate", numTruncate),
    -- Conversion
    ("number->string", numToString),
    -- Numerical Boolean operators
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    -- Scientific functions
    ("sqrt", sciSqrt),
    ("acos", sciAcos),
    ("asin", sciAsin),
    ("atan", sciAtan),
    ("cos", sciCos),
    ("sin", sciSin),
    ("tan", sciTan),
--    ("acosh", sciAcosh),
--    ("asinh", sciAsinh),
--    ("atanh", sciAtanh),
--    ("cosh", sciCosh),
--    ("sinh", sciSinh),
--    ("tanh", sciTanh),
    ("exp", sciExp),
    ("expt", sciExpt),
    ("log", sciLog),
    -- Complex numbers operations
--    ("angle", cAngle),
    ("real-part", cRealPart),
    ("imag-part", cImagPart),
    ("magnitude", cMagnitude),
    ("make-polar", cMakePolar),
--    ("make-rectangular", cMakeRectangular),
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

-- |Absolute value
numAbs :: [LispVal] -> ThrowsError LispVal
numAbs [Number x]   = return $ Number $ abs x
numAbs [Float x]    = return $ Float $ abs x
numAbs [Complex x]  = return $ Complex $ abs x -- Calculates the magnitude 
numAbs [Ratio x]    = return $ Ratio $ abs x
numAbs [x] = throwError $ TypeMismatch "number" x
numAbs l = throwError $ NumArgs 1 l

-- |Ceiling, floor, round and truncate
numCeil, numFloor, numRound, numTruncate :: [LispVal] -> ThrowsError LispVal
numCeil [Number x]   = return $ Number $ ceiling $ fromInteger x
numCeil [Ratio x]    = return $ Number $ ceiling $ fromRational x
numCeil [Float x]    = return $ Number $ ceiling x
numCeil [Complex x]  = 
    if imagPart x == 0 then return $ Number $ ceiling $ realPart x
    else throwError $ TypeMismatch "integer or float" $ Complex x
numCeil [x] = throwError $ TypeMismatch "integer or float" x
numCeil l = throwError $ NumArgs 1 l

numFloor [Number x]   = return $ Number $ floor $ fromInteger x
numFloor [Ratio x]    = return $ Number $ floor $ fromRational x
numFloor [Float x]    = return $ Number $ floor x
numFloor [Complex x]  = 
    if imagPart x == 0 then return $ Number $ floor $ realPart x
    else throwError $ TypeMismatch "integer or float" $ Complex x
numFloor [x] = throwError $ TypeMismatch "integer or float" x
numFloor l = throwError $ NumArgs 1 l

numRound [Number x]   = return $ Number $ round $ fromInteger x
numRound [Ratio x]    = return $ Number $ round $ fromRational x
numRound [Float x]    = return $ Number $ round x
numRound [Complex x]  = 
    if imagPart x == 0 then return $ Number $ round $ realPart x
    else throwError $ TypeMismatch "integer or float" $ Complex x
numRound [x] = throwError $ TypeMismatch "integer or float" x
numRound l = throwError $ NumArgs 1 l

numTruncate [Number x]   = return $ Number $ truncate $ fromInteger x
numTruncate [Ratio x]    = return $ Number $ truncate $ fromRational x
numTruncate [Float x]    = return $ Number $ truncate x
numTruncate [Complex x]  = 
    if imagPart x == 0 then return $ Number $ truncate $ realPart x
    else throwError $ TypeMismatch "integer or float" $ Complex x
numTruncate [x] = throwError $ TypeMismatch "integer or float" x
numTruncate l = throwError $ NumArgs 1 l

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

-- | Boolean operator
numBoolBinop :: (LispVal -> LispVal -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop op []           = throwError $ Default "need at least two arguments"
numBoolBinop op [x]          = throwError $ Default "need at least two arguments"
numBoolBinop op [x, y]       = numCast [x, y] >>= go op
    where 
        go op (List [x@(Number _),  y@(Number _)])      = return $ Bool $ x `op` y
        go op (List [x@(Float _),   y@(Float _)])       = return $ Bool $ x `op` y
        go op (List [x@(Ratio _),   y@(Ratio _)])       = return $ Bool $ x `op` y
        go op (List [x@(Complex _), y@(Complex _)])     = return $ Bool $ x `op` y 
        go op _ = throwError $ Default "unexpected error in boolean operation"

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
numCast [Ratio a,  b@(Complex _)] = 
    return $ List [Complex $ fromInteger (numerator a) / fromInteger (denominator a), b]
-- First number is a complex
numCast [a@(Complex _),  Number b] = return $ List [a, Complex $ fromInteger b] 
numCast [a@(Complex _),  Float b]     = return $ List [a, Complex $ b :+ 0] 
numCast [a@(Complex _),  Ratio b] = 
    return $ List [a, Complex $ fromInteger (numerator b) / fromInteger (denominator b)]
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

-- |Convert a Number to a String
numToString :: [LispVal] -> ThrowsError LispVal
numToString [Number x] = return $ String $ show x
numToString [Float x] = return $ String $ show x
numToString [Ratio x] = return $ String $ show x
numToString [Complex x] = return $ String $ show x
numToString [x] = throwError $ TypeMismatch "number" x
numToString badArgList = throwError $ NumArgs 2 badArgList

-- | Trigonometric functions
sciCos, sciSin, sciTan, sciAcos, sciAsin, sciAtan :: [LispVal] -> ThrowsError LispVal
-- | Cosine of a number
sciCos [Number x] = return $ Float $ cos $ fromInteger x 
sciCos [Float x] = return $ Float $ cos x
sciCos [Ratio x] = return $ Float $ cos $ fromRational x
sciCos [Complex x] = return $ Complex $ cos x
sciCos [notnum] = throwError $ TypeMismatch "number" notnum
sciCos badArgList = throwError $ NumArgs 1 badArgList
-- | Sine of a number
sciSin [Number x] = return $ Float $ sin $ fromInteger x 
sciSin [Float x] = return $ Float $ sin x
sciSin [Ratio x] = return $ Float $ sin $ fromRational x
sciSin [Complex x] = return $ Complex $ sin x
sciSin [notnum] = throwError $ TypeMismatch "number" notnum
sciSin badArgList = throwError $ NumArgs 1 badArgList
-- | Tangent of a number
sciTan [Number x] = return $ Float $ tan $ fromInteger x 
sciTan [Float x] = return $ Float $ tan x
sciTan [Ratio x] = return $ Float $ tan $ fromRational x
sciTan [Complex x] = return $ Complex $ tan x
sciTan [notnum] = throwError $ TypeMismatch "number" notnum
sciTan badArgList = throwError $ NumArgs 1 badArgList
-- | Arccosine of a number
sciAcos [Number x] = return $ Float $ acos $ fromInteger x 
sciAcos [Float x] = return $ Float $ acos x
sciAcos [Ratio x] = return $ Float $ acos $ fromRational x
sciAcos [Complex x] = return $ Complex $ acos x
sciAcos [notnum] = throwError $ TypeMismatch "number" notnum
sciAcos badArgList = throwError $ NumArgs 1 badArgList
-- | Sine of a number
sciAsin [Number x] = return $ Float $ asin $ fromInteger x 
sciAsin [Float x] = return $ Float $ asin x
sciAsin [Ratio x] = return $ Float $ asin $ fromRational x
sciAsin [Complex x] = return $ Complex $ asin x
sciAsin [notnum] = throwError $ TypeMismatch "number" notnum
sciAsin badArgList = throwError $ NumArgs 1 badArgList
-- | Tangent of a number
sciAtan [Number x] = return $ Float $ atan $ fromInteger x 
sciAtan [Float x] = return $ Float $ atan x
sciAtan [Ratio x] = return $ Float $ atan $ fromRational x
sciAtan [Complex x] = return $ Complex $ atan x
sciAtan [notnum] = throwError $ TypeMismatch "number" notnum
sciAtan badArgList = throwError $ NumArgs 1 badArgList

-- #TODO Implement hyperbolic functions
-- Ask teacher
-- | Hyperbolic functions
-- sciAcosh, sciAsinh, sciAtanh, sciCosh, sciSinh, sciTanh :: [LispVal] -> ThrowsError LispVal

-- Misc. Scientific primitives
sciSqrt, sciExp, sciExpt, sciLog :: [LispVal] -> ThrowsError LispVal
-- | Square root of a number
sciSqrt [Number x] = return $ Float $ sqrt $ fromInteger x 
sciSqrt [Float x] = return $ Float $ sqrt x
sciSqrt [Ratio x] = return $ Float $ sqrt $ fromRational x
sciSqrt [Complex x] = return $ Complex $ sqrt x
sciSqrt [notnum] = throwError $ TypeMismatch "number" notnum
sciSqrt badArgList = throwError $ NumArgs 1 badArgList
-- | Return e to the power of x
sciExp [Number x] = return $ Float $ exp $ fromInteger x 
sciExp [Float x] = return $ Float $ exp x
sciExp [Ratio x] = return $ Float $ exp $ fromRational x
sciExp [Complex x] = return $ Complex $ exp x
sciExp [notnum] = throwError $ TypeMismatch "number" notnum
sciExp badArgList = throwError $ NumArgs 1 badArgList
-- | Return x to the power of y
sciExpt [x, y] = numCast [x, y] >>= go
    where 
        go (List [Number x, Number y])      = return $ Number $ round $ (fromInteger x) ** (fromInteger y) 
        go (List [Float x, Float y])        = return $ Float $ x ** y
        go (List [Ratio x, Ratio y])        = return $ Float $ (fromRational x) ** (fromRational y)
        go (List [Complex x, Complex y])    = return $ Complex $ x ** y
        go _ = throwError $ Default "unexpected error in (-)"
sciExpt badArgList = throwError $ NumArgs 2 badArgList
-- | Return the natural logarithm of x
sciLog [Number x] = return $ Float $ log $ fromInteger x 
sciLog [Float x] = return $ Float $ log x
sciLog [Ratio x] = return $ Float $ log $ fromRational x
sciLog [Complex x] = return $ Complex $ log x
sciLog [notnum] = throwError $ TypeMismatch "number" notnum
sciLog badArgList = throwError $ NumArgs 1 badArgList

-- #TODO implement phase (angle)
-- Ask teacher how to convert phase formats from haskell to guile scheme
-- | Complex number functions
cRealPart, cImagPart, cMakePolar, cMagnitude  :: [LispVal] -> ThrowsError LispVal
-- | Real part of a complex number
cRealPart [Number x] = return $ Number $ fromInteger x 
cRealPart [Float x] = return $ Float x
cRealPart [Ratio x] = return $ Float $ fromRational x
cRealPart [Complex x] = return $ Float $ realPart x
cRealPart [notnum] = throwError $ TypeMismatch "number" notnum
cRealPart badArgList = throwError $ NumArgs 1 badArgList
-- | Imaginary part of a complex number
cImagPart [Number x] = return $ Number 0 
cImagPart [Float x] = return $ Number 0
cImagPart [Ratio x] = return $ Number 0
cImagPart [Complex x] = return $ Float $ imagPart x
cImagPart [notnum] = throwError $ TypeMismatch "number" notnum
cImagPart badArgList = throwError $ NumArgs 1 badArgList
-- | Form a complex number from polar components of magnitude and phase.
cMakePolar [mag, p] = numCast [mag, p] >>= go
        where 
            go (List [Number mag, Number p]) 
                | mag == 0 = return $ Number 0   
                | p == 0 = return $ Number mag
                | otherwise = return $ Complex $ mkPolar (fromInteger mag) (fromInteger p)
            go (List [Float mag, Float p])  
                | mag == 0 = return $ Number 0   
                | p == 0 = return $ Float mag
                | otherwise = return $ Complex $ mkPolar mag p
            go (List [Ratio mag, Ratio p]) 
                | mag == 0 = return $ Number 0
                | p == 0 = return $ Float (fromRational mag)
                | otherwise = return $ Complex $ mkPolar (fromRational mag) (fromRational p)
            go val@(List [Complex mag, Complex p]) = throwError $ TypeMismatch "real" val
            go _ = throwError $ Default "unexpected error in make-polar"
cMakePolar badArgList = throwError $ NumArgs 2 badArgList
-- | Return the magnitude (length) of a complex number
cMagnitude [Number x] = return $ Number $ fromInteger x
cMagnitude [Float x] = return $ Float x
cMagnitude [Ratio x] = return $ Float $ fromRational x
cMagnitude [Complex x] = return $ Float $ magnitude x
cMagnitude [notnum] = throwError $ TypeMismatch "number" notnum
cMagnitude badArgList = throwError $ NumArgs 1 badArgList