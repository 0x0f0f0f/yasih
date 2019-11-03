module ImplementationSpec where

import Test.Hspec
--import Test.Quickcheck

import Yasih

-- Testing functions

main :: IO ()
main = hspec spec

spec :: Spec
spec =
    describe "Todo" $ do
        it "todo" $ True `shouldBe` True