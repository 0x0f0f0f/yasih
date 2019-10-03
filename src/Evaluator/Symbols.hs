module Evaluator.Symbols where

import LispTypes
import Environment
import Evaluator.Operators

symbolPrimitives :: [(String, [LispVal] -> ThrowsError LispVal)]
symbolPrimitives =
    [("symbol?", unaryOp symbolp),
    ("string->symbol", unaryOp stringtosymbol),
    ("symbol->string", unaryOp symboltostring)]

symbolp, stringtosymbol, symboltostring :: LispVal -> LispVal
symbolp (Atom _)        = Bool True
symbolp _               = Bool False
stringtosymbol (String s) = Atom s
stringtosymbol _ = Atom ""
symboltostring (Atom s) = String s
symboltostring _ = String ""