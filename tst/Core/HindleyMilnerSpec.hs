{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
module Core.HindleyMilnerSpec
    ( spec
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
import Core.TH                  (coreExpr)
import Core.HindleyMilner       (infer, TypeError(..), HMError)
import Test.Hspec
----------------------------------------------------------------------------------

-- TODO: more tests. preferrably property-based. lol.
spec :: Spec
spec = do
    it "should infer `id 3` :: Int" $
        let g = [ ("id", "a" :-> "a") ]
        in infer g [coreExpr|id 3|] `shouldBe` Right TyInt

    it "should not infer `id 3` when `id` is specialised to `a -> a`" $
        let g = [ ("id", ("a" :-> "a") :-> "a" :-> "a") ]
        in infer g [coreExpr|id 3|] `shouldSatisfy` isUntypedVariableErr

    -- TODO: property-based tests for let
    it "should infer `let x = 3 in id x` :: Int" $
        let g = [ ("id", "a" :-> "a") ]
            e = [coreExpr|let {x = 3} in id x|]
        in infer g e `shouldBe` Right TyInt

    it "should infer `let x = 3; y = 2 in (+#) x y` :: Int" $
        let g = [ ("+#", TyInt :-> TyInt :-> TyInt) ]
            e = [coreExpr|let {x=3;y=2} in (+#) x y|]
        in infer g e `shouldBe` Right TyInt

isUntypedVariableErr :: HMError a -> Bool
isUntypedVariableErr (Left (TyErrCouldNotUnify _ _)) = True
isUntypedVariableErr _                               = False

