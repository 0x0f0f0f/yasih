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
    ("boolean?", unaryOp boolp),
    ("procedure?", unaryOp procp)]

-- |Type testing functions
boolp, procp :: LispVal -> LispVal
boolp (Bool _)          = Bool True
boolp _                 = Bool False
procp (PrimitiveFunc _) = Bool True
procp (IOFunc _)        = Bool True
procp (Func{})          = Bool True
procp _                 = Bool False