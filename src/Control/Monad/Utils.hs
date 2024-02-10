module Control.Monad.Utils
    ( mapAccumLM
    , Kendo(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Tuple               (swap)
import Data.Coerce
import Control.Monad.State
import Control.Monad
----------------------------------------------------------------------------------

-- | Monadic variant of @mapAccumL@

mapAccumLM :: forall m t s a b. (Monad m, Traversable t)
           => (s -> a -> m (s, b))
           -> s
           -> t a
           -> m (s, t b)
mapAccumLM k s t = swap <$> runStateT (traverse k' t) s
    where
        k' :: a -> StateT s m b
        k' a = StateT $ fmap swap <$> flip k a

newtype Kendo m a = Kendo { appKendo :: a -> m a }

instance (Monad m) => Semigroup (Kendo m a) where
    Kendo f <> Kendo g = Kendo (f <=< g)

instance (Monad m) => Monoid (Kendo m a) where
    mempty = Kendo pure

