module Rlp.HindleyMilner
    ( infer
    , check
    , TypeError(..)
    , HMError
    )
    where
--------------------------------------------------------------------------------
import Control.Lens             hiding (Context', Context)
import Control.Monad.Errorful
import Data.Text                qualified as T
import Data.Pretty
import Text.Printf

import Data.Functor
import Control.Comonad.Cofree

import Compiler.RlpcError
import Rlp.AltSyntax
--------------------------------------------------------------------------------

-- | Type error enum.
data TypeError
    -- | Two types could not be unified
    = TyErrCouldNotUnify (Type Name) (Type Name)
    -- | @x@ could not be unified with @t@ because @x@ occurs in @t@
    | TyErrRecursiveType Name (Type Name)
    -- | Untyped, potentially undefined variable
    | TyErrUntypedVariable Name
    | TyErrMissingTypeSig Name
    deriving (Show)

instance IsRlpcError TypeError where
    liftRlpcError = \case
        -- todo: use anti-parser instead of show
        TyErrCouldNotUnify t u -> Text
            [ T.pack $ printf "Could not match type `%s` with `%s`."
                              (rpretty @String t) (rpretty @String u)
            , "Expected: " <> rpretty t
            , "Got: " <> rpretty u
            ]
        TyErrUntypedVariable n -> Text
            [ "Untyped (likely undefined) variable `" <> n <> "`"
            ]
        TyErrRecursiveType t x -> Text
            [ T.pack $ printf "Recursive type: `%s' occurs in `%s'"
                              (rpretty @String t) (rpretty @String x)
            ]

-- | Synonym for @Errorful [TypeError]@. This means an @HMError@ action may
-- throw any number of fatal or nonfatal errors. Run with @runErrorful@.
type HMError = Errorful TypeError

infer = undefined
check = undefined

