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
evalString :: Env -> String -> IO String 
evalString env expr = runIOThrows $ 
    liftM show $ liftThrows (readExpr expr) >>= eval env

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

-- |Evaluate a single expression
runOneExpr :: String -> IO ()
runOneExpr expr = primitiveBindings >>= flip evalAndPrint expr

-- |Takes a filename, loads it and executes it as a program
-- Additional arguments will get bound into a list args within the Scheme program
runProgram :: [String] -> IO ()
runProgram args = do
    -- Take the primitive bindings, bind them in an environment and add a variable args
    -- containing the list of arguments except the program filename
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    -- evaluate a scheme form loading the program and print out errors on stderr
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (head args)]))
        >>= hPutStrLn stderr

-- |REPL
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint