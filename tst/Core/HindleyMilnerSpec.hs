{-# LANGUAGE QuasiQuotes #-}
module Core.HindleyMilnerSpec
    ( spec
    )
    where
----------------------------------------------------------------------------------
import Core.Syntax
import Core.TH                  (coreExpr)
import Core.HindleyMilner       (infer)
import Test.Hspec
----------------------------------------------------------------------------------

-- TODO: more tests. preferrably property-based. lol.
spec :: Spec
spec = do
    it "should infer `id 3` :: Int" $
        let g = [ ("id", TyVar "a" :-> TyVar "a") ]
        in infer g [coreExpr|id 3|] `shouldBe` Right TyInt

