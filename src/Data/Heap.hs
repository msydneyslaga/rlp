module Data.Heap
    ( Heap
    , Addr
    , alloc
    , update
    , free
    , hLookup
    , hLookupUnsafe
    , addresses
    , hSize
    )
    where
----------------------------------------------------------------------------------
import Data.Map                     (Map, (!?))
import Data.Map qualified as M
import Data.List                    (intersect)
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

instance Traversable Heap where
    traverse t (Heap u m) = Heap u <$> (traverse t m)

----------------------------------------------------------------------------------

alloc :: Heap a -> a -> (Heap a, Addr)
alloc (Heap (u:us) m) v = (Heap us (M.insert u v m), u)
alloc (Heap [] _) _     = error "STG heap model ran out of memory..."

update :: Heap a -> Addr -> a -> Heap a
update (Heap u m) k v = Heap u (M.adjust (const v) k m)

free :: Heap a -> Addr -> Heap a
free (Heap u m) k = Heap (k:u) (M.delete k m)

hLookup :: Addr -> Heap a -> Maybe a
hLookup k (Heap _ m) = m !? k

hLookupUnsafe :: Addr -> Heap a -> a
hLookupUnsafe k (Heap _ m) = case m !? k of
    Just a  -> a
    Nothing -> error "erm... segfault much?"

addresses :: Heap a -> [Addr]
addresses (Heap _ m) = M.keys m

hSize :: Heap a -> Int
hSize (Heap _ m) = M.size m

