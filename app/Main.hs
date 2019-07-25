module Main where

--import Control.Monad
import System.Environment
import Numeric

import LispParser
import Evaluator

-- |Parse an expression, now ignoring whitespace
readExpr :: String -> String 
readExpr input = case parseLisp input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value: " ++ (show val)

-- |Parse the first argument
main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)



    
    
    
    
    
    
    