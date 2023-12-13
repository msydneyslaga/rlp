{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Arith
    ( runTestsArith
    ) where
----------------------------------------------------------------------------------
import Data.Functor.Classes             (eq1)
import Core.Syntax
import GM
import Test.QuickCheck
----------------------------------------------------------------------------------

-- does not support division because there are few things i'd hate more than
-- trying to catch divide-by-zero exceptions in pure code
data ArithExpr = IntA Int
               | NegateA ArithExpr
               -- to test Evals
               | IdA ArithExpr
               -- strict arith; these get inlined
               | ArithExpr :+ ArithExpr
               | ArithExpr :* ArithExpr
               | ArithExpr :- ArithExpr
               -- non-strict ops; these get wrapped in an `id` call to test
               -- non-inlined arith
               | ArithExpr ::+ ArithExpr
               | ArithExpr ::* ArithExpr
               | ArithExpr ::- ArithExpr
               deriving Show

evalA :: ArithExpr -> Int
evalA (IntA n)    = n
evalA (IdA  e)    = evalA e
evalA (NegateA a) = negate (evalA a)
evalA (a :+ b)    = evalA a + evalA b
evalA (a :* b)    = evalA a * evalA b
evalA (a :- b)    = evalA a - evalA b
evalA (a ::+ b)   = evalA a + evalA b
evalA (a ::* b)   = evalA a * evalA b
evalA (a ::- b)   = evalA a - evalA b

instance Arbitrary ArithExpr where
    arbitrary = gen 4
        where
            gen :: Int -> Gen ArithExpr
            gen n
                | n > 0 = oneof
                    -- i don't feel like dealing with division at the moment
                    [ IntA <$> int
                    , NegateA <$> arbitrary
                    , IdA <$> arbitrary
                    , b (:+)
                    , b (:-)
                    , b (:*)
                    , b (::+)
                    , b (::-)
                    , b (::*)
                    ]
                | otherwise = IntA <$> int
                where
                    b f = liftA2 f s s
                    s = gen (n `div` 2)
                    -- int = chooseInt (minBound,maxBound)
                    int = chooseInt (-500,500)

prop_ArithExprEqCoreExpr :: ArithExpr -> Bool
prop_ArithExprEqCoreExpr e = arithResult `eq1` coreResult
    where
        arithResult = Just (evalA e)
        coreResult  = evalCore (toCore e)

toCore :: ArithExpr -> Program'
toCore expr = Program
        [ ScDef "id" ["x"] $ Var "x"
        , ScDef "main" [] $ go expr
        ]
    where
        go :: ArithExpr -> Expr'
        go (IntA n)    = LitE (IntL n)
        go (NegateA e) = "negate#" :$ go e
        go (IdA e)     = "id" :$ go e
        go (a :+ b)    = f "+#" a b
        go (a :- b)    = f "-#" a b
        go (a :* b)    = f "*#" a b
        go (a ::+ b)   = f ("id" :$ "+#") a b
        go (a ::- b)   = f ("id" :$ "-#") a b
        go (a ::* b)   = f ("id" :$ "*#") a b

        f n a b = n :$ go a :$ go b

evalCore :: Program' -> Maybe Int
evalCore p = do
    a <- fst <$> evalProg p
    case a of
        (NNum n) -> Just n
        _        -> Nothing

pure []

runTestsArith :: IO Bool
runTestsArith = $quickCheckAll

