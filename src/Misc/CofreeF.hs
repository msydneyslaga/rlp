{-# LANGUAGE PatternSynonyms #-}
module Misc.CofreeF
    ( pattern (:<$)
    )
    where
--------------------------------------------------------------------------------
import Control.Comonad.Trans.Cofree qualified as Trans.Cofree
import Control.Comonad.Trans.Cofree (CofreeF)
--------------------------------------------------------------------------------

pattern (:<$) :: a -> f b -> Trans.Cofree.CofreeF f a b
pattern a :<$ b = a Trans.Cofree.:< b

