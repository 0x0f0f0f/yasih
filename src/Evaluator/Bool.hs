module Evaluator.Bool where 

import LispTypes
import Environment
import Evaluator.Operators

boolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
boolPrimitives = 
    -- Boolean operators
    [("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    -- Type testing functions 
    ("boolean?", unaryOp boolp)]
    
-- |Type testing functions
boolp :: LispVal -> LispVal
boolp (Bool _)          = Bool True
boolp _                 = Bool False