module Evaluator.Primitives where

import LispParser.Atom

-- In Lisp, data types for both code and data are the same
-- This means that this Evaluator returns LispVals

-- |Evaluate Simple LispVals just by returning themselves
-- The val@ notation matches against any LispVal that corresponds
-- To the specified constructor then binds it back into a LispVal
-- The result has type LispVal instead of the matched Constructor
eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
