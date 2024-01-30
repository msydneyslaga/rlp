module Compiler.Types
    ( SrcSpan(..)
    , Located(..)
    , (<<~), (<~>)

    -- * Re-exports
    , Comonad
    , Apply
    , Bind
    )
    where
--------------------------------------------------------------------------------
import Control.Comonad
import Data.Functor.Apply
import Data.Functor.Bind
--------------------------------------------------------------------------------

-- | Token wrapped with a span (line, column, absolute, length)
data Located a = Located SrcSpan a
    deriving (Show, Functor)

instance Apply Located where
    liftF2 f (Located sa p) (Located sb q)
            = Located (sa <> sb) (p `f` q)

instance Bind Located where
    Located sa a >>- k = Located (sa <> sb) b
        where
            Located sb b = k a

instance Comonad Located where
    extract (Located _ a) = a
    extend ck w@(Located p _) = Located p (ck w)

data SrcSpan = SrcSpan
    !Int -- ^ Line
    !Int -- ^ Column
    !Int -- ^ Absolute
    !Int -- ^ Length
    deriving Show

instance Semigroup SrcSpan where
    SrcSpan la ca aa sa <> SrcSpan lb cb ab sb = SrcSpan l c a s where
        l = min la lb
        c = min ca cb
        a = min aa ab
        s = case aa `compare` ab of
            EQ -> max sa sb
            LT -> max sa (ab + lb - aa)
            GT -> max sb (aa + la - ab)

-- | A synonym for '(<<=)' with a tighter precedence and left-associativity for
-- use with '(<~>)' in a sort of, comonadic pseudo-applicative style.

(<<~) :: (Comonad w) => (w a -> b) -> w a -> w b
(<<~) = (<<=)

infixl 4 <<~

-- | Similar to '(<*>)', but with a cokleisli arrow.

(<~>) :: (Comonad w, Bind w) => w (w a -> b) -> w a -> w b
mc <~> ma = mc >>- \f -> ma =>> f

infixl 4 <~>

