module LispRepl where

import LispTypes
import Evaluator
import Environment
import LispParser
import Control.Monad.Except

import System.IO hiding (try) -- Hiding try because of Parsec try usage

-- |Prints out a string and immediately flush the stream.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Prints a prompt and read a line of input
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- |Pull the code to parse, evaluate a string and trap the errors out of main
-- Reads a list of expressions from a string, maps eval over it, checks if the result
-- list is empty and return an empty list (nil)
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $
    (liftThrows . readExprList) expr
    >>= liftM nilOrLast . mapM (eval env)
    >>= return . show
    where nilOrLast x = if null x then List [] else last x

-- |Evaluate a string and print the result

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

-- |Custom loop for REPL. Monadic functions that repeats but does not return a
-- value. until_ takes a predicate that signals when to stop, an action to
-- perform before the test and a function that returns an action to apply to
-- the input.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return () -- If the predicate returns true stop
        -- Else run action then recurse-loop
        else action result >> until_ pred prompt action

-- |Try to load the standard library from an include path list
loadStdlib :: [String] -> Env -> IO ()
loadStdlib ipaths env = do
    let loaded = False
    mapM_  go ipaths
    where go p = runIOThrows (liftM show $ eval env (List [Atom "load", String (p ++ "stdlib.scm")]))

-- |Evaluate a single expression
runOneExpr :: [String] -> String -> IO ()
runOneExpr ipaths expr = do
    env <- primitiveBindings
    loadStdlib ipaths env
    evalAndPrint env expr

-- |Takes a filename, loads it and executes it as a program
-- Additional arguments will get bound into a list args within the Scheme program
runProgram :: [String] -> [String] -> IO ()
runProgram ipaths args = do
    -- Take the primitive bindings, bind them in an environment and add a variable args
    -- containing the list of arguments except the program filename
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    loadStdlib ipaths env
    -- evaluate a scheme form loading the program and print out errors on stderr
    runIOThrows (liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

-- |REPL
runRepl :: [String] -> IO ()
runRepl ipaths = do
    env <- primitiveBindings
    loadStdlib ipaths env
    until_ (== "quit") (readPrompt "λ> ") (evalAndPrint env)