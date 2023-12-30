module Core.ParseSpec
    ( spec
    )
    where
----------------------------------------------------------------------------------
import CoreSyntax
import Core.Syntax
import Compiler.JustRun
import Compiler.RlpcError
import Control.Monad                ((<=<))
import Data.Coerce
import Data.Text                    qualified as T
import Data.Functor.Classes         (Eq1(..))
import Test.Hspec
import Test.QuickCheck
----------------------------------------------------------------------------------

spec :: Spec
spec = do
    it "should be a right-inverse to the unparser\
        \up to source code congruency" $
        property $ \p -> (unparse <=< parse) p ~== Right p

    -- TODO: abitrary ASTs
    -- it "should be a right-inverse to the unparser\
    --     \up to source code congruency" $
    --     property $ \p -> (parse <=< unparse) p == Right p

(~==) :: (Eq1 f) => f ProgramSrc -> f ProgramSrc -> Bool
(~==) = liftEq congruentSrc

infix 4 ~==

parse :: ProgramSrc -> Either RlpcError Program'
parse (ProgramSrc s) = justParseSrc (T.unpack s)

unparse :: Program' -> Either RlpcError ProgramSrc
unparse = Right . unparseCoreProg

