module Main (main) where
----------------------------------------------------------------------------------
import Control.Monad
import System.Exit
import Test.QuickCheck
import Arith
----------------------------------------------------------------------------------

runTests :: IO Bool
runTests = runTestsArith

main :: IO ()
main = do
    good <- runTests
    if good
    then exitSuccess
    else exitFailure

