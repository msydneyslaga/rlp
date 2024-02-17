{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}
module Compiler.Types
    ( SrcSpan(..)
    , srcSpanLine, srcSpanColumn, srcSpanAbs, srcSpanLen
    , Located(..)
    , HasLocation(location)
    , _Located
    , nolo

    , (<~>), (~>)

    -- * Re-exports
    , Comonad
    , Apply
    , Bind
    )
    where
--------------------------------------------------------------------------------
import Language.Haskell.TH.Syntax   (Lift)

import Control.Comonad
import Control.Comonad.Cofree
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Semigroup.Foldable
import Data.Kind
import Control.Lens                 hiding ((<<~))

import Data.List.NonEmpty           (NonEmpty)
--------------------------------------------------------------------------------

-- | Token wrapped with a span (line, column, absolute, length)
data Located a = Located SrcSpan a
    deriving (Show, Lift, Functor)

class GetLocation s where
    srcspan :: s -> SrcSpan

class HasLocation s where
    location :: Lens' s SrcSpan

(<~>) :: a -> b -> SrcSpan
(<~>) = undefined

infixl 5 <~>

(~>) :: a -> b -> b
(~>) = undefined

infixl 4 ~>

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
    deriving (Show, Eq, Lift)

tupling :: Iso' SrcSpan (Int, Int, Int, Int)
tupling = iso (\ (SrcSpan a b c d) -> (a,b,c,d))
              (\ (a,b,c,d) -> SrcSpan a b c d)

srcSpanLine, srcSpanColumn, srcSpanAbs, srcSpanLen :: Lens' SrcSpan Int
srcSpanLine = tupling . _1
srcSpanColumn = tupling . _2
srcSpanAbs = tupling . _3
srcSpanLen = tupling . _4

-- | debug tool
nolo :: a -> Located a
nolo = Located (SrcSpan 0 0 0 0)

instance Semigroup SrcSpan where
    -- multiple identities? what are the consequences of this...?
    SrcSpan _ _ _ 0 <> SrcSpan l c a s = SrcSpan l c a s
    SrcSpan l c a s <> SrcSpan _ _ _ 0 = SrcSpan l c a s

    SrcSpan la ca aa sa <> SrcSpan lb cb ab sb = SrcSpan l c a s where
        l = min la lb
        c = min ca cb
        a = min aa ab
        s = case aa `compare` ab of
            EQ -> max sa sb
            LT -> max sa (ab + sb - aa)
            GT -> max sb (aa + sa - ab)

makePrisms ''Located

--------------------------------------------------------------------------------

instance (GetLocation a) => GetLocation (NonEmpty a) where
    srcspan = foldMap1 srcspan

instance GetLocation SrcSpan where
    srcspan = id

instance (Functor f) => GetLocation (Cofree f SrcSpan) where
    srcspan = extract

--------------------------------------------------------------------------------

instance HasLocation SrcSpan where
    location = id

instance (Functor f) => HasLocation (Cofree f SrcSpan) where
    location = _extract

