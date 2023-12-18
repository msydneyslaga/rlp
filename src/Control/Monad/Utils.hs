module Control.Monad.Utils
    ( mapAccumLM
    )
    where
----------------------------------------------------------------------------------
import Data.Tuple               (swap)
import Control.Monad.State
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

