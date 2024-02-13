module Rlp.Parse.Associate
    {-# WARNING "unimplemented" #-}
    ( associate
    )
    where
--------------------------------------------------------------------------------
import Data.HashMap.Strict              qualified as H
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Functor.Const
import Data.Functor
import Data.Text                        qualified as T
import Text.Printf
import Control.Lens
import Rlp.Parse.Types
import Rlp.Syntax
--------------------------------------------------------------------------------

associate :: OpTable -> Decl' RlpcPs -> Decl' RlpcPs
associate _ p = p

{-# WARNING associate "unimplemented" #-}

examplePrecTable :: OpTable
examplePrecTable = H.fromList
    [ ("+",    (InfixL,6))
    , ("*",    (InfixL,7))
    , ("^",    (InfixR,8))
    , (".",    (InfixR,7))
    , ("~",    (Infix, 9))
    , ("=",    (Infix, 4))
    , ("&&",   (Infix, 3))
    , ("||",   (Infix, 2))
    , ("$",    (InfixR,0))
    , ("&",    (InfixL,0))
    ]

