module Spec where

import Test.Hspec
--import Test.Quickcheck

import LispRepl

trapCatch x = extractValue $ trapError x

evaluate x = do 
    evaluated <- return $ liftM show $ readExpr x >>= eval
    return $ extractValue $ trapError evaluated

-- Testing functions

main :: IO ()
main = putStrLn "Tests not implemented yet"
    
    -- hspec $ do
    -- describe "LispParser" $ 
    --     it "tests are not yet implemented" $
    --     trapCatch (readExpr "(+ 2 2)") `shouldBe` Number 4
