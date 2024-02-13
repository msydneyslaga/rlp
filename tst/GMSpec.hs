{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module GMSpec
    ( spec
    )
    where
----------------------------------------------------------------------------------
import Test.Hspec
import Arith
import Test.QuickCheck
import GM                   (Node(..), resultOf, resultOfExpr)
import Core.TH
import Core.Examples        qualified as Ex
----------------------------------------------------------------------------------

spec :: Spec
spec = do
    it "should correctly evaluate 3-2 with inlining" $ do
        resultOf [coreProg|main = (-#) 3 2;|] `shouldBe` Just (NNum 1)

    it "should correctly evaluate 3-2 without inlining" $ do
        resultOf [coreProg|id x = x; main = (id (-#)) 3 2;|] `shouldBe` Just (NNum 1)

    it "should correctly evaluate arbitrary arithmetic" $ do
        property $ \e ->
            let arithRes = Just (evalArith e)
                coreRes  = evalCore e
            in coreRes `shouldBe` arithRes

    describe "test programs" $ do
        it "fac 3" $
            resultOf Ex.fac3 `shouldBe` Just (NNum 6)

        it "sum [1,2,3]" $
            resultOf Ex.sumList `shouldBe` Just (NNum 6)

        it "k 3 ((/#) 1 0)" $
            resultOf Ex.constDivZero `shouldBe` Just (NNum 3)

        it "id (case ... of { ... })" $
            resultOf Ex.idCase `shouldBe` Just (NNum 5)

        it "bool pattern matching with named constructors" $
            resultOf Ex.namedBoolCase `shouldBe` Just (NNum 123)

        it "list pattern matching with named constructors" $
            resultOf Ex.namedConsCase `shouldBe` Just (NNum 6)


