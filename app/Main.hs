module Main where

import Control.Monad
import System.Environment
import Numeric

import LispParser

-- |Parse the first argument
main :: IO ()
main = do
    args <- getArgs
    putStrLn $ readExpr (args !! 0)



    
    
    
    
    
    
    