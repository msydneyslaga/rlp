{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Core.HindleyMilnerSpec
    ( spec
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
import Core.TH                  (coreExpr)
import Core.HindleyMilner
import Control.Monad.Errorful
import Data.Either              (isLeft)
import Test.Hspec
----------------------------------------------------------------------------------

-- TODO: more tests. preferrably property-based. lol.
spec :: Spec
spec = do
    it "should infer `id 3` :: Int" $
        let g = [ ("id", "a" :-> "a") ]
        in infer' g [coreExpr|id 3|] `shouldBe` Right TyInt

    it "should not infer `id 3` when `id` is specialised to `a -> a`" $
        let g = [ ("id", ("a" :-> "a") :-> "a" :-> "a") ]
        in infer' g [coreExpr|id 3|] `shouldSatisfy` isLeft

    -- TODO: property-based tests for let
    it "should infer `let x = 3 in id x` :: Int" $
        let g = [ ("id", "a" :-> "a") ]
            e = [coreExpr|let {x = 3} in id x|]
        in infer' g e `shouldBe` Right TyInt

    it "should infer `let x = 3; y = 2 in (+#) x y` :: Int" $
        let g = [ ("+#", TyInt :-> TyInt :-> TyInt) ]
            e = [coreExpr|let {x=3;y=2} in (+#) x y|]
        in infer' g e `shouldBe` Right TyInt
    
    it "should find `3 :: Bool` contradictory" $
        let e = [coreExpr|3|]
        in check' [] (TyCon "Bool") e `shouldSatisfy` isLeft

    it "should infer `fix ((+#) 1)` :: Int" $
        let g = [ ("fix", ("a" :-> "a") :-> "a")
                , ("+#", TyInt :-> TyInt :-> TyInt) ]
            e = [coreExpr|fix ((+#) 1)|]
        in infer' g e `shouldBe` Right TyInt

    it "should infer mutually recursively defined lists" $
        let g = [ ("cons", TyInt :-> TyCon "IntList" :-> TyCon "IntList") ]
            e :: Expr'
            e = [coreExpr|letrec { as = cons 1 bs; bs = cons 2 as } in as|]
        in infer' g e `shouldBe` Right (TyCon "IntList")

infer' :: Context' -> Expr' -> Either [TypeError] Type
infer' g e = case runErrorful $ infer g e of
    (Just t,   _) -> Right t
    (Nothing, es) -> Left es

check' :: Context' -> Type -> Expr' -> Either [TypeError] ()
check' g t e = case runErrorful $ check g t e of
    (Just t,   _) -> Right ()
    (Nothing, es) -> Left es

