{-# LANGUAGE TemplateHaskell #-}
module Compiler.Types
    ( SrcSpan(..)
    , srcspanLine, srcspanColumn, srcspanAbs, srcspanLen
    , Located(..)
    , _Located
    , nolo
    , (<<~), (<~>), (<#>)

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
import Control.Lens             hiding ((<<~))
import Language.Haskell.TH.Syntax   (Lift)
--------------------------------------------------------------------------------

-- | Token wrapped with a span (line, column, absolute, length)
data Located a = Located SrcSpan a
    deriving (Show, Lift, Functor)

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
    deriving (Show, Lift)

tupling :: Iso' SrcSpan (Int, Int, Int, Int)
tupling = iso (\ (SrcSpan a b c d) -> (a,b,c,d))
              (\ (a,b,c,d) -> SrcSpan a b c d)

srcspanLine, srcspanColumn, srcspanAbs, srcspanLen :: Lens' SrcSpan Int
srcspanLine = tupling . _1
srcspanColumn = tupling . _2
srcspanAbs = tupling . _3
srcspanLen = tupling . _4

-- | debug tool
nolo :: a -> Located a
nolo = Located (SrcSpan 0 0 0 0)

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

-- this is getting silly

(<#>) :: (Functor f) => f (a -> b) -> a -> f b
fab <#> a = fmap ($ a) fab

infixl 4 <#>

makePrisms ''Located

