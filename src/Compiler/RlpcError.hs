module Compiler.RlpcError
    ( RlpcError(..)
    , IsRlpcError(..)
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Errorful
----------------------------------------------------------------------------------

data RlpcError = RlpcErr String -- temp
    deriving (Show, Eq)

class IsRlpcError a where
    liftRlpcErr :: a -> RlpcError

