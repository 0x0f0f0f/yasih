module NumericalSpec (main, spec) where

import Yasih.LispTypes
import Yasih.Evaluator
import Yasih

import Test.Hspec
import Test.Tasty
import Test.Tasty.QuickCheck
import System.IO.Unsafe
import Data.Complex
import Data.Ratio

main :: IO ()
main = hspec spec

-- | Exec with local standard library
ev = evalOne ["stdlib/stdlib.scm"]

spec :: Spec
spec = describe "Numerical Primitives" $ do
    describe "Arithmetic Primitives" $ do
        describe "Work correctly with different types" $ do
            it "+" $
                ev "(+ 1 2.4 3/2 3-4i)" `shouldReturn` (Right $ Complex (7.9 :+ (-4)))
