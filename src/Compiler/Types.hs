{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances, QuantifiedConstraints #-}
{-# LANGUAGE PatternSynonyms #-}
module Compiler.Types
    ( SrcSpan(..)
    , srcSpanLine, srcSpanColumn, srcSpanAbs, srcSpanLen
    , pattern (:<$)
    , Located(..)
    , HasLocation(..)
    , _Located
    , nolo, nolo'

    , (<~>), (~>), (~~>), (<~~)

    , comb2, comb3, comb4
    , lochead

    -- * Re-exports
    , Comonad(extract)
    , Apply
    , Bind
    )
    where
--------------------------------------------------------------------------------
import Language.Haskell.TH.Syntax   (Lift)

import Control.Comonad
import Control.Comonad.Cofree
import Control.Comonad.Trans.Cofree qualified as Trans.Cofree
import Control.Comonad.Trans.Cofree (CofreeF)
import Data.Functor.Apply
import Data.Functor.Bind
import Data.Functor.Compose
import Data.Functor.Foldable
import Data.Semigroup.Foldable
import Data.Fix                     hiding (cata, ana)
import Data.Kind
import Control.Lens                 hiding ((<<~), (:<))

import Data.List.NonEmpty           (NonEmpty)
import Data.Function                (on)
--------------------------------------------------------------------------------

-- | Token wrapped with a span (line, column, absolute, length)
data Located a = Located SrcSpan a
    deriving (Show, Lift, Functor)

data Floc f = Floc SrcSpan (f (Floc f))

pattern (:<$) :: a -> f b -> Trans.Cofree.CofreeF f a b
pattern a :<$ b = a Trans.Cofree.:< b

(<~>) :: a -> b -> SrcSpan
(<~>) = undefined

infixl 5 <~>

-- (~>) :: (CanGet k, CanSet k', HasLocation k a, HasLocation k' b)
--      => a -> b -> b
-- a ~> b = 
(~>) = undefined

infixl 4 ~>

-- (~~>) :: (CanGet k, HasLocation k a, CanSet k', HasLocation k' b)
--       => (a -> b) -> a -> b
-- (~~>) :: (f SrcSpan -> b) -> Cofree f SrcSpan -> Cofree f SrcSpan
-- f ~~> (ss :< as) = ss :< f as
(~~>) = undefined

infixl 3 ~~>

-- (<~~) :: (GetLocation a, HasLocation b) => (a -> b) -> a -> b
-- a <~~ b = a b & location <>~ srcspan b
(<~~) = undefined

infixr 2 <~~

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

nolo' :: f (Cofree f SrcSpan) -> Cofree f SrcSpan
nolo' as = SrcSpan 0 0 0 0 :< as

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

--------------------------------------------------------------------------------

data GetOrSet = Get | Set | GetSet

class CanGet (k :: GetOrSet)
class CanSet (k :: GetOrSet) where

instance CanGet Get
instance CanGet GetSet
instance CanSet Set
instance CanSet GetSet

data GetSetLens (k :: GetOrSet) s t a b :: Type where
    Getter_      :: (s -> a)             -> GetSetLens Get s t a b
    Setter_      :: ((a -> b) -> s -> t) -> GetSetLens Set s t a b
    GetterSetter :: (CanGet k', CanSet k')
                 => (s -> a) -> (s -> b -> t) -> GetSetLens k' s t a b

type GetSetLens' k s a = GetSetLens k s s a a

class HasLocation k s | s -> k where
    -- location :: (Access k f, Functor f) => LensLike' f s SrcSpan
    getSetLocation :: GetSetLens' k s SrcSpan

type family Access (k :: GetOrSet) f where
    Access GetSet f = Functor f
    Access Set f = Settable f
    Access Get f = (Functor f, Contravariant f)

instance HasLocation GetSet SrcSpan where
    getSetLocation = GetterSetter id (flip const)
    -- location = fromGetSetLens getSetLocation

instance (CanSet k, HasLocation k a) => HasLocation Set (r -> a) where
    getSetLocation = Setter_ $ \ss ra r -> ra r & fromSet getSetLocation %~ ss
    -- location = fromSet getSetLocation

instance (HasLocation k a) => HasLocation k (Cofree f a) where
    getSetLocation = case getSetLocation @_ @a of
        Getter_ sa -> Getter_ $ \ (s :< _) -> sa s
        Setter_ abst -> Setter_ $ \ss (s :< as) -> abst ss s :< as
        GetterSetter sa sbt -> GetterSetter sa' sbt' where
            sa' (s :< _) = sa s
            sbt' (s :< as) b = sbt s b :< as

location :: (Access k f, Functor f, HasLocation k s)
         => LensLike' f s SrcSpan
location = fromGetSetLens getSetLocation

fromGetSetLens :: (Access k f, Functor f) => GetSetLens' k s a -> LensLike' f s a
fromGetSetLens gsl = case gsl of
    Getter_ sa          -> to sa
    Setter_ abst        -> setting abst
    GetterSetter sa sbt -> lens sa sbt

fromGet :: (CanGet k) => GetSetLens k s t a b -> Getter s a
fromGet (Getter_ sa)        = to sa
fromGet (GetterSetter sa _) = to sa

fromSet :: (CanSet k) => GetSetLens k s t a b -> Setter s t a b
fromSet (Setter_ abst)        = setting abst
fromSet (GetterSetter sa sbt) = lens sa sbt

fromGetSet :: (CanGet k, CanSet k) => GetSetLens k s t a b -> Lens s t a b
fromGetSet (GetterSetter sa sbt) = lens sa sbt

--------------------------------------------------------------------------------

comb2 :: (Functor f, Semigroup m)
      => (Cofree f m -> Cofree f m -> f (Cofree f m))
      -> Cofree f m -> Cofree f m -> Cofree f m
comb2 f a b = ss :< f a b
    where ss = a `mextract` b

comb3 :: (Functor f, Semigroup m)
      => (Cofree f m -> Cofree f m -> Cofree f m -> f (Cofree f m))
      -> Cofree f m -> Cofree f m -> Cofree f m -> Cofree f m
comb3 f a b c = ss :< f a b c
    where ss = a `mapply` b `mextract` c

comb4 :: (Functor f, Semigroup m)
      => (Cofree f m -> Cofree f m -> Cofree f m -> Cofree f m
         -> f (Cofree f m))
      -> Cofree f m -> Cofree f m -> Cofree f m -> Cofree f m -> Cofree f m
comb4 f a b c d = ss :< f a b c d
    where ss = a `mapply` b `mapply` c `mextract` d

mextract :: (Comonad w, Semigroup m) => w m -> w m -> m
mextract = (<>) `on` extract

mapply :: (Comonad w, Semigroup m) => w m -> w m -> w m
mapply a b = b <&> (<> extract a)

lochead :: Functor f
        => (f SrcSpan -> f SrcSpan) -> Located (f SrcSpan) -> Cofree f SrcSpan
lochead afs (Located ss fss) = ss :< unwrap (lochead afs $ Located ss fss)

--------------------------------------------------------------------------------

makePrisms ''Located
