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

-- #TODO eval remaining types: complex, ratio, vector, list
-- and dottedlist

-- Function application clause
-- func : args = a list which 1st element is func and the 
-- remaining tail is args. Run eval recursively over args
-- And then apply the function func over the resulting list
eval (List (Atom func : args)) = apply func $ map eval args

-- |Apply a function defined in a primitives table
-- Look for func into the primitives table then return 
-- The corresponding function if found, otherwise default
-- To the False value 
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False)
    ($ args) -- Apply it to the arguments
    $ lookup func primitives -- Look for the function

-- |Primitive functions table
primitives :: [(String, [LispVal] -> LispVal)]
primitives = 
    -- Binary Numerical operations
    [("+", numericBinop (+)), 
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    
    -- Type testing functions
    ("symbol?", unaryOp symbolp), 
    ("number?", unaryOp numberp),
    ("float?", unaryOp floatp),
    ("string?", unaryOp stringp),
    ("char?", unaryOp charp),
    ("bool?", unaryOp boolp),
    ("ratio?", unaryOp ratiop),
    ("complex?", unaryOp complexp),
    ("list?", unaryOp listp),
    ("vector?", unaryOp vectorp),

    -- Symbol handling functions
    ("symbol->string", unaryOp symboltostring),
    ("string->symbol", unaryOp stringtosymbol)]


-- #TODO extend numericBinop to allow numeric operations on complex
-- numbers and ratios

-- |Take a primitive Haskell Integer function and wrap it
-- with code to unpack an argument list, apply the function to it
-- and return a numeric value
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op 
    $ map unpackNum params

-- |Unpack numbers from LispValues
unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum (String n) = let parsed = reads n in
    if null parsed then 0 else fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

-- |Apply an unary operator 
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v

-- |Type testing functions
symbolp, numberp, floatp, stringp, charp, boolp, ratiop, complexp, listp, vectorp:: LispVal -> LispVal
symbolp (Atom _)        = Bool True
symbolp _               = Bool False
numberp (Number _)      = Bool True
numberp _               = Bool False
floatp (Float _)        = Bool True
floatp _                = Bool False
stringp (String _)      = Bool True
stringp _               = Bool False
charp (Character _)     = Bool True
charp _                 = Bool False
boolp (Bool _)          = Bool True
boolp _                 = Bool False
ratiop (Ratio _)        = Bool True
ratiop _                = Bool False
complexp (Complex _)    = Bool True
complexp _              = Bool False
listp (List _)          = Bool True
listp (DottedList _ _)  = Bool True
listp _                 = Bool False
vectorp (Vector _)      = Bool True
vectorp _               = Bool False

-- |Symbol handling functions
symboltostring, stringtosymbol :: LispVal -> LispVal
symboltostring (Atom s) = String s
symboltostring _ = String ""
stringtosymbol (String s) = Atom s
stringtosymbol _ = Atom ""
