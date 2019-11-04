module Yasih.Evaluator.Extension where

-- Primitives that are not in R5RS

import Yasih.LispTypes
import Yasih.Environment
import Yasih.Evaluator.Operators

extensionPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
extensionPrimitives =
    [("dump", unaryOp dump)]


-- | Dump a function code
dump :: LispVal -> LispVal
dump x = String $ dumpVal x