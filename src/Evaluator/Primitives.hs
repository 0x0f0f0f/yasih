{-# LANGUAGE ExistentialQuantification #-}
module Evaluator.Primitives where

import LispParser.Atom
import Evaluator.Errors
import Evaluator.ListPrimitives


-- |Evaluate expressions. Returns a monadic ThrowsError value
-- In Lisp, data types for both code and data are the same
-- This means that this Evaluator returns a value of type ThrowsError LispVal


-- The val@ notation matches against any LispVal that corresponds
-- To the specified constructor then binds it back into a LispVal
-- The result has type LispVal instead of the matched Constructor
eval :: LispVal -> ThrowsError LispVal
eval val@(String _)             = return val
eval val@(Number _)             = return val
eval val@(Float _)              = return val
eval val@(Character _)          = return val
eval val@(Bool _)               = return val
eval val@(Complex _)            = return val
eval val@(Ratio _)              = return val
eval val@(Vector _)             = return val
eval (List [Atom "quote", val]) = return val

-- If-clause. #f is false and any other value is considered true
eval (List [Atom "if", pred, conseq, alt]) = do 
    result <- eval pred 
    -- Evaluate pred, if it is false eval alt, if true eval conseq
    case result of 
        Bool False -> eval alt 
        Bool True -> eval conseq
        badArg -> throwError $ TypeMismatch "boolean" badArg 

-- Function application clause
-- func : args = a list with func as head and args as tail 
-- Run eval recursively over args then apply func over the resulting list
eval (List (Atom func : args))  = mapM eval args >>= apply func

-- Bad form clause
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm


-- #TODO eval remaining types:  vector, 

-- |Apply a function defined in a primitives table
-- apply func args
-- Look for func into the primitives table then return 
-- the corresponding function if found, otherwise throw an error
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args) -- Apply it to the arguments
    $ lookup func primitives -- Look for the function

-- |Primitive functions table
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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
    ("string->symbol", unaryOp stringtosymbol),
    
    -- Numerical Boolean operators
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),

    -- Boolean operators
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    
    -- String Boolean operators
    ("string=?", strBoolBinop (==)),
    ("string?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    
    -- List primitives
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", equal)]

-- |Apply an unary operator 
unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp f [v] = return $ f v

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

-- |Unpack numbers from LispValues
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
    if null parsed then throwError $ TypeMismatch "number" $ String n
    else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

-- |Unpack strings from LispVal
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s 
unpackStr (Bool s ) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- |Unpack a Bool value from a LispVal
unpackBool :: LispVal -> ThrowsError Bool 
unpackBool (Bool b) = return b 
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

-- #TODO extend numericBinop to allow numeric operations on complex
-- numbers and ratios

-- |Take a primitive Haskell Integer function and wrap it
-- with code to unpack an argument list, apply the function to it
-- and return a numeric value
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
-- Throw an error if there's only one argument
numericBinop op val@[_] = throwError $ NumArgs 2 val
-- Fold the operator leftway if there are enough args
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op 
    
-- |Apply an operator to two arguments and return a Bool
-- boolBinop unpacker operator arguments
-- unpacker is used to unpack the arguments from LispVals to native types
-- op performs the boolean operation

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
    then throwError $ NumArgs 2 args
    else do 
        left <- unpacker $ args !! 0
        right <- unpacker $ args !! 1
        -- Op function is used as an infix operator by wrapping it in backticks
        return $ Bool $ left `op` right 

-- | Type specific boolean operators
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool


-- Equivalence primitive functions

-- |eqv checks for the equivalence of two items
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool x), (Bool y)]           = return $ Bool $ x == y
eqv [(Number x), (Number y)]       = return $ Bool $ x == y
eqv [(Float x), (Float y)]         = return $ Bool $ x == y
eqv [(Ratio x), (Ratio y)]         = return $ Bool $ x == y
eqv [(Complex x), (Complex y)]     = return $ Bool $ x == y
eqv [(Character x), (Character y)] = return $ Bool $ x == y
eqv [(String x), (String y)]       = return $ Bool $ x == y
eqv [(Atom x), (Atom y)]           = return $ Bool $ x == y

-- eqv clause for Dotted list builds a full list and calls itself on it
eqv [(DottedList xs x), (DottedList ys y)]
    = eqv [List $ xs ++ [x], List $ ys ++ [y]] 

-- use the helper function eqvList using eqv to compare pair by pair
eqv [l1@(List x), l2@(List y)] = eqvList eqv [l1, l2]
eqv [_, _] = return $ Bool False 
eqv badArgList = throwError $ NumArgs 2 badArgList

-- |Helper function to check for the equivalence of a pair of items
eqvPair (j, k) = case eqv [j, k] of
    Left err -> False 
    Right (Bool val) -> val

-- |Data type that can hold any function to a LispVal into a native type
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

-- |Helper function that takes an Unpacker and determines if two LispVals
-- are equal before unpacking them
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool 
unpackEquals x y (AnyUnpacker unpacker) = do 
    unpacked1 <- unpacker x 
    unpacked2 <- unpacker y
    return $ unpacked1 == unpacked2 
    `catchError` (const $ return False)

-- |Check equivalence of two items with weak typing 
-- (equal? 2 "2") should return #t while (eqv? 2 "2") => #f
-- This approach uses Existential Types, a ghc extension that
-- allows for heterogenous lists subject to typeclass constraints

equal :: [LispVal] -> ThrowsError LispVal
-- use the helper function eqvList using equal to compare pair by pair
equal [l1@(List x), l2@(List y)] = eqvList equal [l1, l2]
-- eqv clause for Dotted list builds a full list and calls itself on it
equal [(DottedList xs x), (DottedList ys y)]
    = equal [List $ xs ++ [x], List $ ys ++ [y]] 
equal [x, y] = do 
    -- Make an heterogenous list of [unpackNum, unpackStr, unpackBool]\
    -- and then map the partially applied unpackEquals over it, giving a list
    -- of Bools. We use 'or' to return true if any one of them is true.
    primitiveEquals <- liftM or $ mapM (unpackEquals x y) 
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    -- Simply test the two arguments with eqv?, since eqv? is stricter than
    -- equal? return true whenever eqv? does
    eqvEquals <- eqv [x, y]
    -- Return a disjunction of eqvEquals and primitiveEquals
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- |Helper function that checks for the equivalence of items in two lists
-- accepts a function as the first argument to allow for both strong/weak equivalence
eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFunc [(List x), (List y)] = return $ Bool $ (length x == length y)
        && (all eqvPair $ zip x y )