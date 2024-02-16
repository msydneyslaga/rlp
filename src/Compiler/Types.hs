{-# LANGUAGE TemplateHaskell #-}
module Compiler.Types
    ( SrcSpan(..)
    , srcSpanLine, srcSpanColumn, srcSpanAbs, srcSpanLen
    , Located(..)
    , _Located
    , located
    , nolo

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
import Control.Lens                 hiding ((<<~))
import Language.Haskell.TH.Syntax   (Lift)
--------------------------------------------------------------------------------

-- | Token wrapped with a span (line, column, absolute, length)
data Located a = Located SrcSpan a
    deriving (Show, Lift, Functor)

located :: Lens (Located a) (Located b) a b
located = lens extract ($>)

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

