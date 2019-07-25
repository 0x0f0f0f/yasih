module Evaluator.Show where

import LispParser

import Data.Ratio
import Data.Complex

-- |Print the content of a LispVal
-- Pattern matching is used to destructure
-- The algebraic data type selecting a clause
-- Based on constructors
showVal :: LispVal -> String
-- Basic Types
showVal (Atom name) = name
showVal (String s) = "\"" ++ s ++ "\""
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Character c) = show c
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"

-- Composite types
showVal (Ratio r) = 
    (show . numerator) r ++ "/" ++ (show . denominator) r
showVal (Complex c) = 
    (show . realPart) c ++ "+" ++ (show . imagPart) c ++ "i"
showVal (List l) = 
    "(" ++ unwordsList l ++ ")"
showVal (DottedList head tail) = 
    "(" ++ unwordsList head ++ "." ++ showVal tail ++ ")"


-- |Helper function mapping showVal over a Lisp List
-- |Basically unwords for LispVal

{- 
 unwordsList is defined in point-free style
 no argument is specified and it is written only in 
 terms of function composition and partial application:
 map is partially applied to showVal (creates a fun that
 takes a list of LispVals and returns a list of their
 string representation). Then, it is composed to 
 Haskell's unwords which merges strings in a list 
 into a single string separated by spaces.

 This is an important example of currying
-}
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Defining the show method for LispVal
-- See https://www.haskell.org/tutorial/classes.html 
-- for details on typeclasses
instance Show LispVal where show = showVal


