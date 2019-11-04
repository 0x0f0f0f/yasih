{-# LANGUAGE FlexibleContexts #-}
module Yasih.Evaluator where

import Yasih.LispTypes
import Yasih.LispParser
import Yasih.Environment
import Yasih.Evaluator.Operators
import Yasih.Evaluator.Numerical
import Yasih.Evaluator.Char
import Yasih.Evaluator.Strings
import Yasih.Evaluator.Lists
import Yasih.Evaluator.Bool
import Yasih.Evaluator.Symbols
import Yasih.Evaluator.Equivalence
import Yasih.Evaluator.IO
import Yasih.Evaluator.Vector
import Yasih.Evaluator.Extension

import Data.IORef
import Data.Maybe
import Control.Monad.Except



-- |Evaluate expressions. Returns a monadic IOThrowsError value
-- In Lisp, data types for both code and data are the same
-- This means that this Evaluator returns a value of type IOThrowsError LispVal


-- The val@ notation matches against any LispVal that corresponds
-- To the specified constructor then binds it back into a LispVal
-- The result has type LispVal instead of the matched Constructor
eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(Number _) = return val
eval env val@(Float _) = return val
eval env val@(Ratio x)
    | x == 0 = return $ Number 0
    | otherwise = return val
eval env val@(Complex _) = return val
eval env val@(String _) = return val
eval env val@(Character _) = return val
eval env val@(Bool _) = return val
eval env val@(Vector _) = return val
eval env (List [Atom "quote", val]) = return val

-- Get a variable
eval env (Atom id) = getVar env id

-- Access the current lexical environment
eval env (List [Atom "env"]) = do
    vars <- getVars env
    return $ List $ map toPair vars
    where toPair (var, val) = List [Atom var, val]

-- | load a file and evaluate its contents. A special form
-- is used because apply does not accept an env binding but
-- statements in the loaded file can affect the top level environment
eval env (List [Atom "load", String filename]) = do
    loadedFile <- loadHelper filename
    if null loadedFile then return $ List []
    else (liftM last . mapM (eval env)) loadedFile

-- Set a variable
eval env (List [Atom "set!", Atom var, form]) =
    checkReserved var >>
    eval env form >>= setVar env var

-- Define a variable
eval env (List [Atom "define", Atom var, form]) =
    checkReserved var >>
    eval env form >>= defineVar env var

-- Define a macro
eval env (List (Atom "define-syntax" : List (Atom var : params) : body)) =
    makeMacro env params body >>= defineVar env var

-- Evaluate quasiquotation. AKA macro expander.
-- Recursively evaluate unquote forms, or just return the items if not unquoted
eval env (List [Atom "quasiquote", form]) =
    evalUnquotes form
    where evalUnquotes form =
            case form of
                List [Atom "unquote", form] -> eval env form
                List items -> do
                    results <- traverse evalUnquotes items
                    return $ List results
                _ -> return form

-- Define a function
-- (define (f x y) (+ x y)) => (lamda ("x" "y") ...)
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    checkReserved var >>
    makeNormalFunc env params body >>= defineVar env var


-- Define a variable argument function
-- (define (func a b) c . body)
eval env (List (Atom "define" : DottedList (Atom var : params)  varargs : body)) =
    checkReserved var >>
    makeVarargs varargs env params body >>= defineVar env var

-- λλλ Lambda functions! λλλ
-- (lambda (a b c) (+ a b c))
eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body

-- (lambda (a b c . d) body)
eval env (List (Atom "lambda" : DottedList params varargs: body)) =
    makeVarargs varargs env params body

-- (lambda args body)
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body


-- If-clause. #f is false and any other value is considered true
eval env (List [Atom "if", pred, conseq, alt]) = do
    result <- eval env pred
    -- Evaluate pred, if it is false eval alt, if true eval conseq
    case result of
        Bool False -> eval env alt
        Bool True -> eval env conseq
        badArg -> throwError $ TypeMismatch "boolean" badArg

-- If-clause without an else
eval env (List [Atom "if", pred, conseq]) = do
    result <- eval env pred
    -- Evaluate pred, if it is false eval alt, if true eval conseq
    case result of
        Bool False -> return $ List []
        Bool True -> eval env conseq
        badArg -> throwError $ TypeMismatch "boolean" badArg


-- cond clause: test each one of the alts clauses and eval the first
-- which test evaluates to true, otherwise eval the 'else' clause
-- Example: (cond ((> 3 2) 'greater) ((< 3 2) 'less) (else 'equal))
-- Evaluates to the atom greater.
-- see https://schemers.org/Documents/Standards/R5RS/HTML/r5rs-Z-H-7.html#%_sec_4.2.1
eval env form@(List (Atom "cond" : clauses)) = if null clauses
    then throwError $ BadSpecialForm "No true clause in cond expression" form
    else case head clauses of
        List (Atom "else" : exprs) ->
            if null exprs then return $ List []
            else mapM (eval env) exprs >>= liftThrows . return . last
        -- Piggy back the evaluation of the clauses on the already
        -- Existing if clause.
        List [test, expr] -> eval env $ List [Atom "if", test, expr,
            -- If test is not true, recurse the evaluation of
            -- cond on the remaining clauses
            List (Atom "cond" : tail clauses)]
        _ -> throwError $ BadSpecialForm "Ill-formed clause in cond expression" form


-- case expression
-- Evaluate a key expression and iterate over ((<datum1>, ...) expr) clauses
-- To check if the key value appears at least once in the datums list.
-- If so, evaluate that clause.
-- Example:
-- (case (* 2 3)
--   ((2 3 5 7) 'prime)
--   ((1 4 6 8 9) 'composite))             ===>  composite
eval env form@(List (Atom "case" : (key : clauses))) = if null clauses
    then throwError $ BadSpecialForm "No true clause in case expression" form
    else case head clauses of
        List (Atom "else" : exprs) ->
            if null exprs then return $ List []
            else mapM (eval env) exprs >>= liftThrows . return . last
        List (List datums : exprs) -> do
            keyValue <- eval env key -- Evaluate the key
            -- Iterate over datums to check for an equal one
            equality <- mapM (\x -> liftThrows (eqv [keyValue, x])) datums
            if Bool True `elem` equality
                then if null exprs then return $ List []
                    else mapM (eval env) exprs >>= liftThrows . return . last
                else eval env $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "Ill-formed clause in case expression" form


-- Function application clause
-- Run eval recursively over args then apply func over the resulting list
eval env (List (function : args)) = do
    func <- eval env function
    case func of
        Func {isMacro = True} -> apply func args >>= eval env
        _ -> mapM (eval env) args >>= apply func


-- Bad form clause
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

-- |Apply a function defined in a primitives table
-- apply func args
-- Look for func into the primitives table then return
-- the corresponding function if found, otherwise throw an error
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func isMacro params varargs body closure) args
    -- Throw error if argument number is wrong
    | num params /= num args && isNothing varargs = throwError $ NumArgs (num params) args
    -- Bind arguments to a new env and execute statements
    -- Zip together parameter names and already evaluated args
    -- together into a list of pairs, then create
    -- a new environment for the function closure
    | otherwise = liftIO (bindVars closure $ zip params args)
        >>= bindVarArgs varargs >>= evalBody
    where
        remainingArgs = drop (length params) args
        num = toInteger . length
        -- Map the monadic function eval env over every statement in the func body
        evalBody env = liftM last $ mapM (eval env) body
        -- Bind variable argument list to env if present
        bindVarArgs arg env = case arg of
            Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
            Nothing -> return env
apply badForm args = throwError $ NotFunction "Not a function: " $ show badForm

-- |Take an initial null environment, make name/value pairs and bind
-- primitives into the new environment
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>=
    flip bindVars (map (makeFunc IOFunc) Yasih.Evaluator.ioPrimitives ++ map (makeFunc PrimitiveFunc) primitives)
    where makeFunc constructor (var, func) = (var, constructor func)

-- |Primitive functions table
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    numericalPrimitives ++
    charPrimitives ++
    stringPrimitives ++
    boolPrimitives ++
    symbolPrimitives ++
    listPrimitives ++
    equivalencePrimitives ++
    extensionPrimitives ++ 
    vectorPrimitives

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives = ("apply", applyProc) : Yasih.Evaluator.IO.ioPrimitives

-- | Wrapper around apply responsible for destructuring the argument list
-- into the form apply expects
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc [func, DottedList args rest] = applyProc [func, List (args ++ [rest]) ]
applyProc (func : args) = apply func args


reservedKeywords = ["define", "load", "if", "cond", "case", "apply", "lambda", "set!"]
checkReserved :: String -> IOThrowsError ()
checkReserved var
    | var `elem` reservedKeywords = throwError $ ReservedKeyword var
    | otherwise = case lookup var primitives of
        Just a -> throwError $ ReservedKeyword var
        Nothing -> return ()