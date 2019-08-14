import Test.Hspec
--import Test.Quickcheck

import LispParser
import Evaluator

-- |Parse an expression
-- readExpr input
-- Parse and evaluate a LispVal returning a monadic value
readExpr :: String -> ThrowsError LispVal 
readExpr input = case parseLisp input of
    Left err -> throwError $ Parser err
    Right val -> return val

evaluate x = do 
    evaluated <- return $ liftM show $ readExpr x >>= eval
    return $ extractValue $ trapError evaluated


-- Testing functions

main :: IO ()
main = putStrLn "Tests are not yet implemented"
{- main = hspec $ do
    describe "LispParser" $ do 
        it "tests are not yet implemented" 
        -}