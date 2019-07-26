module Main where

--import Control.Monad
import System.Environment
import Numeric

import LispParser
import Evaluator

-- |Parse an expression
readExpr :: String -> LispVal 
readExpr input = case parseLisp input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

-- |Parse and eval the first argument
main :: IO ()
main = getArgs >>= putStrLn . show . eval . readExpr . (!! 0)



    
    
    
    
    
    
    