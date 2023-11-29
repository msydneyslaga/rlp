{-
Module : Data.Heap
Description : A model heap used by abstract machine
-}
module Data.Heap
    ( Heap
    , Addr
    , alloc
    , update
    , adjust
    , free
    , hLookup
    , hLookupUnsafe
    , addresses
    , hSize
    , hView
    , hViewUnsafe
    , mapWithAddress
    , elems
    , assocs
    , foldrWithAddress
    , foldMapWithAddress
    )
    where
----------------------------------------------------------------------------------
import Data.Map                     (Map, (!?))
import Debug.Trace
import Data.Map qualified as M
import Data.List                    (intersect)
import GHC.Stack                    (HasCallStack)
----------------------------------------------------------------------------------

data Heap a = Heap [Addr] (Map Addr a)
    deriving Show

type Addr = Int

instance Semigroup (Heap a) where
    Heap ua ma <> Heap ub mb = Heap u m
        where
            m = ma `M.union` mb
            u = ua `intersect` ub

instance Monoid (Heap a) where
    mempty = Heap [0..] mempty

instance Functor Heap where
    fmap f (Heap u m) = Heap u (fmap f m)

instance Foldable Heap where
    foldr f z (Heap u m) = foldr f z m

    null (Heap _ m) = M.size m == 0

    length (Heap _ m) = M.size m

instance Traversable Heap where
    traverse t (Heap u m) = Heap u <$> (traverse t m)

----------------------------------------------------------------------------------

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap (u:us) m) v = (Heap us (M.insert u v m), u)
alloc (Heap [] _)     _ = error "STG heap model ran out of memory..."

update :: Addr -> a -> Heap a -> Heap a
update k v (Heap u m) = Heap u (M.adjust (const v) k m)

adjust :: Addr -> (a -> a) -> Heap a -> Heap a
adjust k f (Heap u m) = Heap u (M.adjust f k m)

free :: Addr -> Heap a -> Heap a
free k (Heap u m) = Heap (k:u) (M.delete k m)

hLookup :: Addr -> Heap a -> Maybe a
hLookup k (Heap _ m) = m !? k

hLookupUnsafe :: (HasCallStack) => Addr -> Heap a -> a
hLookupUnsafe k (Heap _ m) = case m !? k of
    Just v  -> v
    Nothing -> error $ "erm... segfault much? addr: " <> show k

addresses :: Heap a -> [Addr]
addresses (Heap _ m) = M.keys m

-- | Intended for use with view patterns
hView :: Heap a -> Addr -> Maybe a
hView = flip hLookup

-- | Intended for use with view patterns
hViewUnsafe :: Heap a -> Addr -> a
hViewUnsafe = flip hLookupUnsafe

mapWithAddress :: (Addr -> a -> b) -> Heap a -> Heap b
mapWithAddress f (Heap u m) = Heap u (M.mapWithKey f m)

foldrWithAddress :: (Addr -> a -> b -> b) -> b -> Heap a -> b
foldrWithAddress f z h = foldr (uncurry f) z $ assocs h

foldMapWithAddress :: (Monoid m) => (Addr -> a -> m) -> Heap a -> m
foldMapWithAddress f h = mconcat $ fmap (uncurry f) (assocs h)

elems :: Heap a -> [a]
elems (Heap _ m) = M.elems m

assocs :: Heap a -> [(Addr,a)]
assocs (Heap _ m) = M.assocs m

