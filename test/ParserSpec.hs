module ParserSpec (main, spec) where

import Yasih.LispParser
import Yasih.LispTypes


import Test.Hspec
import System.IO.Unsafe

main :: IO ()
main = hspec spec


spec :: Spec
spec =
    -- From https://github.com/write-you-a-scheme-v2/scheme/blob/master/test-hs/Spec.hs
    describe "Parsing Correctness" $ do
        it "Atom" $
            readExpr "bb-8?" `shouldBe` (Right $ Atom "bb-8?")

        it "Num Negative" $
            readExpr "-2187" `shouldBe` (Right $ Number (-2187))

        it "Num Positive" $
            readExpr "112233" `shouldBe` (Right $ Number 112233)

        it "Num Positive with Sign" $
            readExpr "+12345" `shouldBe` (Right $ Number 12345)

        it "String" $
            readExpr "\"Gen L Organa\"" `shouldBe` (Right $ String "Gen L Organa")

        it "Bool True" $
            readExpr "#t" `shouldBe` (Right $ Bool True)

        it "Bool False" $
            readExpr "#f" `shouldBe` (Right $ Bool False)

        it "Nil" $
            readExpr "'()" `shouldBe` (Right $ List [Atom "quote", List []])

        it "S-Expr: homogenous list" $
            readExpr "(2 1 87)" `shouldBe`
            (Right $ List [Number 2, Number 1,Number 87])

        it "S-Expr: homogenous list quoted" $
            readExpr "'(2 1 87)" `shouldBe`
            (Right $ List [Atom "quote",List [Number 2, Number 1,Number 87]])

        it "S-Expr: heterogenous list" $
            readExpr "(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
            (Right $ List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87])

        it "S-Expr: heterogenous list quoted" $
            readExpr "'(stromTrooper \"Fn\" 2 1 87)" `shouldBe`
            (Right $ List [Atom "quote", List [Atom "stromTrooper", String "Fn", Number 2, Number 1,Number 87]])

        it "S-Expr: single negative" $
            readExpr "(-42)" `shouldBe` (Right $ List [Number (-42)])

        it "S-Expr: (- num)" $
            readExpr "(- 42)" `shouldBe` (Right $ List [Atom "-", Number 42])

        it "S-Expr: prim call: numbers" $
            readExpr "(+ 1 2)" `shouldBe`
            (Right $ List [Atom "+", Number 1, Number 2])

        it "S-Expr: prim call: neg nums" $
            readExpr "(- -42 -42)" `shouldBe`
            (Right $ List [Atom "-", Number (-42), Number (-42)])

        it "S-Expr: prim call: atoms" $
            readExpr "(- rogue squadron)" `shouldBe`
            (Right $ List [Atom "-", Atom "rogue", Atom "squadron"])

        it "S-Expr: nested list" $
            readExpr "(lambda (x x) (+ x x))" `shouldBe`
            (Right $ List [Atom "lambda", List [Atom "x", Atom "x"], List [Atom "+", Atom "x", Atom "x"]])

        it "Comment: end-of/single line" $
            readExpr ";skip\nartoodetoo ;extra will throw\n;skip" `shouldBe` (Right $ Atom "artoodetoo")

        it "Comment: multi-line line" $
            readExpr "{-Han\nShot\nFirst\n-} (c3 {- these are not the droids you're looking for-} po)\n {-Jar Jar Binks =?= Sith Lord -}" `shouldBe` (Right $ List [Atom "c3",Atom "po"])
