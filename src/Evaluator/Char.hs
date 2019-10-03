module Evaluator.Char where

import LispTypes
import Environment
import Evaluator.Operators

import Data.Char
import Control.Monad.Except

charPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
charPrimitives =
    [("char?", unaryOp charp), -- Char predicates
    ("char-alphabetic?", charIsAlpha),
    ("char-numeric?", charIsNumeric),
    ("char-whitespace?", charIsWhitespace),
    ("char-upper-case?", charIsUpper),
    ("char-lower-case?", charIsLower),
    ("char-is-both?", charIsBoth),
    ("char-upcase", charToUpper), -- Char modifiers
    ("char-downcase", charToLower),
    ("char->integer", charToInteger), -- Char conversion
    ("integer->char", charToInteger),
    ("char=?", charBoolBinop (==)), -- Char Comparison
    ("char<?", charBoolBinop (<)),
    ("char>?", charBoolBinop (>)),
    ("char<=?", charBoolBinop (<=)),
    ("char>=?", charBoolBinop (>=)),
    ("char-ci=?", ciCharBoolBinop (==)),
    ("char-ci<?", ciCharBoolBinop (<)),
    ("char-ci>?", ciCharBoolBinop (>)),
    ("char-ci<=?", ciCharBoolBinop (<=)),
    ("char-ci>=?", ciCharBoolBinop (>=))]

-- |Type testing functions
charp :: LispVal -> LispVal
charp (Character _)     = Bool True
charp _                 = Bool False

-- |Character predicates
charIsAlpha, charIsNumeric, charIsWhitespace, charIsLower, charIsUpper, charIsBoth :: [LispVal] -> ThrowsError LispVal
charIsAlpha [Character x] = return $ Bool $ isAlpha x
charIsAlpha [notchar] = throwError $ TypeMismatch "char" notchar
charIsAlpha badArgList = throwError $ NumArgs 1 badArgList
charIsNumeric [Character x] = return $ Bool $ isNumber x
charIsNumeric [notchar] = throwError $ TypeMismatch "char" notchar
charIsNumeric badArgList = throwError $ NumArgs 1 badArgList
charIsWhitespace [Character x] = return $ Bool $ isSpace x
charIsWhitespace [notchar] = throwError $ TypeMismatch "char" notchar
charIsWhitespace badArgList = throwError $ NumArgs 1 badArgList
charIsLower [Character x] = return $ Bool $ isLower x
charIsLower [notchar] = throwError $ TypeMismatch "char" notchar
charIsLower badArgList = throwError $ NumArgs 1 badArgList
charIsUpper [Character x] = return $ Bool $ isUpper x
charIsUpper [notchar] = throwError $ TypeMismatch "char" notchar
charIsUpper badArgList = throwError $ NumArgs 1 badArgList
charIsBoth [Character x] = return $ Bool $ isUpper x || isLower x
charIsBoth [notchar] = throwError $ TypeMismatch "char" notchar
charIsBoth badArgList = throwError $ NumArgs 1 badArgList

-- |Character modifiers
charToLower, charToUpper :: [LispVal] -> ThrowsError LispVal
charToLower [Character x] = return $ Character $ toLower x
charToLower [notchar] = throwError $ TypeMismatch "char" notchar
charToLower badArgList = throwError $ NumArgs 1 badArgList
charToUpper [Character x] = return $ Character $ toUpper x
charToUpper [notchar] = throwError $ TypeMismatch "char" notchar
charToUpper badArgList = throwError $ NumArgs 1 badArgList

-- Character conversion
-- |Get the ASCII/Unicode integer value of a character
charToInteger :: [LispVal] -> ThrowsError LispVal
charToInteger [Character x] = return $ Number $ toInteger $ ord x
charToInteger [notchar] = throwError $ TypeMismatch "char" notchar
charToInteger badArgList = throwError $ NumArgs 1 badArgList

-- |Get the character corresponding to the integer in the ASCII/Unicode table
integerToChar :: [LispVal] -> ThrowsError LispVal
integerToChar [Number x] = return $ Character $ chr $ fromInteger x
integerToChar [notinteger] = throwError $ TypeMismatch "integer" notinteger
integerToChar badArgList = throwError $ NumArgs 1 badArgList
