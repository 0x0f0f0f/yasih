module Main where

--import Control.Monad
import System.Environment
import Numeric

import LispParser
import Evaluator

-- |Parse an expression
-- readExpr input
-- Parse and evaluate a LispVal returning a monadic value
readExpr :: String -> ThrowsError LispVal 
readExpr input = case parseLisp input of
    Left err -> throwError $ Parser err
    Right val -> return val

-- |Parse and eval the first argument
main :: IO ()
main = do
    args <- getArgs 
    evaluated <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaluated

    
    
    
    
    
    
    