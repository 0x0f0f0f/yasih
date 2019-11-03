module R5rsTest where

import Test.Hspec
--import Test.Quickcheck

import Yasih

trapCatch x = extractValue $ trapError x

evaluate x = do 
    evaluated <- return $ liftM show $ readExpr x >>= eval
    return $ extractValue $ trapError evaluated

-- Testing functions

main :: IO ()
main = do

