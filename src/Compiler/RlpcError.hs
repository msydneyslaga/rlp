module Compiler.RlpcError
    ( RlpcError(..)
    , IsRlpcError(..)
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Errorful
----------------------------------------------------------------------------------

data RlpcError = RlpcErr String -- temp
    deriving Show

class IsRlpcError a where
    liftRlpcErr :: a -> RlpcError

