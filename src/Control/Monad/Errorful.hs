{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections, PatternSynonyms #-}
module Control.Monad.Errorful
    ( ErrorfulT
    , runErrorfulT
    , Errorful
    , runErrorful
    , mapErrorful
    , MonadErrorful(..)
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Coerce
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as H
import Lens.Micro
----------------------------------------------------------------------------------

newtype ErrorfulT e m a = ErrorfulT { runErrorfulT :: m (Maybe a, [e]) }

type Errorful e = ErrorfulT e Identity

pattern Errorful :: (Maybe a, [e]) -> Errorful e a
pattern Errorful a = ErrorfulT (Identity a)

runErrorful :: Errorful e a -> (Maybe a, [e])
runErrorful m = coerce (runErrorfulT m)

class (Applicative m) => MonadErrorful e m | m -> e where
    addWound :: e -> m ()
    addFatal :: e -> m a

instance (Applicative m) => MonadErrorful e (ErrorfulT e m) where
    addWound e = ErrorfulT $ pure (Just (), [e])
    addFatal e = ErrorfulT $ pure (Nothing, [e])

instance MonadTrans (ErrorfulT e) where
    lift m = ErrorfulT ((\x -> (Just x,[])) <$> m)

instance (MonadIO m) => MonadIO (ErrorfulT e m) where
    liftIO = lift . liftIO

instance (Functor m) => Functor (ErrorfulT e m) where
    fmap f (ErrorfulT m) = ErrorfulT (m & mapped . _1 . _Just %~ f)

instance (Applicative m) => Applicative (ErrorfulT e m) where
    pure a = ErrorfulT . pure $ (Just a, [])

    ErrorfulT m <*> ErrorfulT n = ErrorfulT $ m `apply` n where
        apply :: m (Maybe (a -> b), [e]) -> m (Maybe a, [e]) -> m (Maybe b, [e])
        apply = liftA2 $ \ (mf,e1) (ma,e2) -> (mf <*> ma, e1 <> e2)

instance (Monad m) => Monad (ErrorfulT e m) where
    ErrorfulT m >>= k = ErrorfulT $ do
        (a,es) <- m
        case a of
            Just x      -> runErrorfulT (k x)
            Nothing     -> pure (Nothing, es)

mapErrorful :: (Functor m) => (e -> e') -> ErrorfulT e m a -> ErrorfulT e' m a
mapErrorful f (ErrorfulT m) = ErrorfulT $
    m & mapped . _2 . mapped %~ f

-- when microlens-pro drops we can write this as
--    mapErrorful f = coerced . mapped . _2 . mappd %~ f
-- lol

