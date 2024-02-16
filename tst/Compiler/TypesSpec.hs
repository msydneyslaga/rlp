{-# LANGUAGE ParallelListComp #-}
module Compiler.TypesSpec
    ( spec
    )
    where
--------------------------------------------------------------------------------
import Control.Lens.Combinators
import Data.Function            ((&))

import Test.QuickCheck
import Test.Hspec

import Compiler.Types           (SrcSpan(..), srcSpanAbs, srcSpanLen)
--------------------------------------------------------------------------------

spec :: Spec
spec = do
    describe "SrcSpan" $ do
        -- it "associates under closure"
        --     prop_SrcSpan_mul_associative
        it "commutes under closure"
            prop_SrcSpan_mul_commutative
        it "equals itself when squared"
            prop_SrcSpan_mul_square_eq

prop_SrcSpan_mul_associative :: Property
prop_SrcSpan_mul_associative = property $ \a b c ->
    -- very crudely approximate when overflow will occur; bail we think it
    -- will
    (([a,b,c] :: [SrcSpan]) & allOf (each . (srcSpanAbs <> srcSpanLen))
                                    (< (maxBound @Int `div` 3)))
    ==> (a <> b) <> c === a <> (b <> c :: SrcSpan)

prop_SrcSpan_mul_commutative :: Property
prop_SrcSpan_mul_commutative = property $ \a b ->
    a <> b === (b <> a :: SrcSpan)

prop_SrcSpan_mul_square_eq :: Property
prop_SrcSpan_mul_square_eq = property $ \a ->
    a <> a === (a :: SrcSpan)

instance Arbitrary SrcSpan where
    arbitrary = do
        l <- chooseInt (1, maxBound)
        c <- chooseInt (1, maxBound)
        a <- chooseInt (0, maxBound)
            `suchThat` (\n -> n >= pred l + pred c)
        s <- chooseInt (0, maxBound)
        pure $ SrcSpan l c a s

    shrink (SrcSpan l c a s) =
        [ SrcSpan l' c' a' s'
        | (l',c',a',s') <- shrinkParts
        , l' >= 1
        , c' >= 1
        , a' >= pred l' + pred c'
        ]
      where
        -- shfl as = unsafePerformIO (generate $ shuffle as)
        shrinkParts = 
            [ (l',c',a',s')
            | l' <- shrinkIntegral l
            | c' <- shrinkIntegral c
            | a' <- shrinkIntegral a
            | s' <- shrinkIntegral s
            ]

