module Main where

import LispRepl

import System.IO
import System.Environment
import Numeric



-- |Parse and eval the first argument
-- Or enter into a REPL Loop
main :: IO ()
main = do
    args <- getArgs 
    case args of 
        [] -> runRepl -- No argument is passed => run the REPL
        [filename] -> runProgram $ args
        ["-e", expr] -> runOneExpr expr
        _ -> do
            hPutStrLn stderr "Usage: haskell-toy-scheme [EXPR]"
            hPutStrLn stderr "If EXPR is provided evaluate it. Otherwise run the REPL."