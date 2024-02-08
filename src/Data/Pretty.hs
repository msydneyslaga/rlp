{-# LANGUAGE OverloadedStrings #-}
module Data.Pretty
    ( Pretty(..)
    )
    where
----------------------------------------------------------------------------------
import Data.String      (IsString(..))
----------------------------------------------------------------------------------

class Pretty a where
    -- pretty :: a -> ISeq
    -- prettyPrec :: a -> Int -> ISeq

    -- {-# MINIMAL pretty | prettyPrec #-}
    -- pretty a = prettyPrec a 0
    -- prettyPrec a _ = iBracket (pretty a)

