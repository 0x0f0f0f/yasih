module Main where

--import Control.Monad
import System.Environment
import Numeric

import LispRepl


-- |Parse and eval the first argument
-- Or enter into a REPL Loop
main :: IO ()
main = do
    args <- getArgs 
    case length args of 
        0 -> runRepl -- No argument is passed => run the REPL
        1 -> evalAndPrint $ args !! 0
        _ -> do
            putStrLn "Usage: haskell-toy-scheme [EXPR]"
            putStrLn "If EXPR is provided evaluate it. Otherwise run the REPL."