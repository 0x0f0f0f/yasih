module LispRepl (
    module Evaluator,
    module LispParser,
    module LispRepl
) where 

import Evaluator
import LispParser

import System.IO hiding (try) -- Hiding try because of Parsec try usage

-- |Parse an expression
-- readExpr input
-- Parse and evaluate a LispVal returning a monadic value
readExpr :: String -> ThrowsError LispVal 
readExpr input = case parseLisp input of
    Left err -> throwError $ Parser err
    Right val -> return val

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

-- |Eval an expression initializing a null environment
runOne :: String -> IO ()
runOne expr = primitiveBindings >>= flip evalAndPrint expr

-- |REPL
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint