{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Compiler.RLPC
    ( RLPC(..)
    )
    where

-- TODO: fancy errors
newtype RLPC a = RLPC { runRLPC :: Either String a }
    deriving (Functor, Applicative, Monad)

