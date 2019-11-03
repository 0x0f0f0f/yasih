module Yasih where

import Yasih.LispTypes
import Yasih.Evaluator
import Yasih.Environment
import Yasih.LispParser

import Control.Monad.Except
import Data.Char (isSpace)
import Data.List (isPrefixOf)
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
evalString :: Env -> String -> IOThrowsError LispVal
evalString env expr =
    (liftThrows . readExprList) expr
    >>= liftM nilOrLast . mapM (eval env)
    where nilOrLast x = if null x then List [] else last x

-- |Evaluate a string and print the result
evalStringAndShow :: Env -> String -> IO String
evalStringAndShow env expr = runIOThrows $
    (liftThrows . readExprList) expr
    >>= liftM nilOrLast . mapM (eval env)
    >>= return . show
    where nilOrLast x = if null x then List [] else last x

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalStringAndShow env expr >>= putStrLn


-- |Custom loop for REPL. Monadic functions that repeats but does not return a
-- value. replLoop takes a predicate that signals when to stop, an action to
-- perform before the test and a function that returns an action to apply to
-- the input.
-- #TODO move parsing logic to parser!
replLoop :: Monad m => m String -> (String -> m ()) -> m ()
replLoop prompt action = do
    result <- prompt
    case dropWhile isSpace result of
        "quit" -> return ()
        "exit" -> return ()
        ';' : xs -> replLoop prompt action
        "" -> replLoop prompt action
        '#' : '|' : xs : '|' : '#' : ys -> replLoop (return ys) action
        _ -> action result >> replLoop prompt action
    
-- |Try to load the standard library from an include path list
loadStdlib :: [String] -> Env -> IO ()
loadStdlib ipaths env = do
    let loaded = False
    mapM_  go ipaths
    where go p = runIOThrows (liftM show $ eval env (List [Atom "load", String (p ++ "stdlib.scm")]))

-- |Evaluate and print a single expression
runOne :: [String] -> String -> IO ()
runOne ipaths expr = do
    env <- primitiveBindings
    loadStdlib ipaths env
    evalAndPrint env expr

-- |Evaluate a single expression
evalOne :: [String] -> String -> IO (ThrowsError LispVal)
evalOne ipaths expr = do
    env <- liftIO primitiveBindings
    liftIO $ loadStdlib ipaths env
    runExceptT $ evalString env expr

-- |Takes a filename, loads it and executes it as a program
-- Additional arguments will get bound into a list args within the Scheme program
evalProgram :: [String] -> [String] -> IO (ThrowsError LispVal)
evalProgram ipaths args = do
    -- Take the primitive bindings, bind them in an environment and add a variable args
    -- containing the list of arguments except the program filename
    env <- liftIO (primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)])
    liftIO $ loadStdlib ipaths env
    -- evaluate a scheme form loading the program and print out errors on stderr
    runExceptT $ eval env (List [Atom "load", String (head args)])

-- |Takes a filename, loads it and executes it as a program, printing the results
-- Additional arguments will get bound into a list args within the Scheme program
runProgram :: [String] -> [String] -> Bool -> IO ()
runProgram ipaths args showResult = do
    -- Take the primitive bindings, bind them in an environment and add a variable args
    -- containing the list of arguments except the program filename
    env <-  primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    loadStdlib ipaths env
    let program = List [Atom "load", String (head args)] in
    -- evaluate a scheme form loading the program and print out errors on stderr
      runIOThrows (if showResult then liftM show (eval env program) else eval env program >> return "")
        >>= hPutStrLn stderr

-- |REPL
runRepl :: [String] -> IO ()
runRepl ipaths = do
    env <- primitiveBindings
    loadStdlib ipaths env
    replLoop (readPrompt "λ> ") (evalAndPrint env)
