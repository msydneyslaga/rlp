module Rlp.Parse.Associate
    ( associate
    )
    where
--------------------------------------------------------------------------------
import Data.HashMap.Strict              qualified as H
import Data.Functor.Foldable
import Data.Functor.Const
import Lens.Micro
import Rlp.Parse.Types
import Rlp.Syntax
--------------------------------------------------------------------------------

associate :: OpTable -> RlpExpr' RlpcPs -> RlpExpr' RlpcPs
associate pt e = undefined

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

