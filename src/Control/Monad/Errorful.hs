{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TupleSections, PatternSynonyms #-}
module Control.Monad.Errorful
    ( ErrorfulT
    , runErrorfulT
    , Errorful
    , runErrorful
    , mapErrors
    , MonadErrorful(..)
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Coerce
import Lens.Micro
----------------------------------------------------------------------------------

newtype ErrorfulT e m a = ErrorfulT { runErrorfulT :: m (Either e (a, [e])) }

type Errorful e = ErrorfulT e Identity

pattern Errorful :: (Either e (a, [e])) -> Errorful e a
pattern Errorful a = ErrorfulT (Identity a)

runErrorful :: Errorful e a -> Either e (a, [e])
runErrorful m = coerce (runErrorfulT m)

class (Applicative m) => MonadErrorful e m | m -> e where
    addWound   :: e -> m ()
    addFatal   :: e -> m a

    -- not sure if i want to add this yet...
    -- catchWound :: m a -> (e -> m a) -> m a

instance (Applicative m) => MonadErrorful e (ErrorfulT e m) where
    addWound e = ErrorfulT $ pure . Right $ ((), [e])
    addFatal e = ErrorfulT $ pure . Left  $ e

instance MonadTrans (ErrorfulT e) where
    lift m = ErrorfulT (Right . (,[]) <$> m)

instance (MonadIO m) => MonadIO (ErrorfulT e m) where
    liftIO = lift . liftIO

instance (Functor m) => Functor (ErrorfulT e m) where
    fmap f (ErrorfulT m) = ErrorfulT $ fmap (_1 %~ f) <$> m

instance (Applicative m) => Applicative (ErrorfulT e m) where
    pure a = ErrorfulT (pure . Right $ (a, []))

    m <*> a = ErrorfulT (m' `apply` a')
        where
            m' = runErrorfulT m
            a' = runErrorfulT a
            -- TODO: strict concatenation
            apply = liftA2 $ liftA2 (\ (f,e1) (x,e2) -> (f x, e1 ++ e2)) 

instance (Monad m) => Monad (ErrorfulT e m) where
    ErrorfulT m >>= k = ErrorfulT $ do
        m' <- m
        case m' of
            Right (a,es) -> runErrorfulT (k a)
            Left e       -> pure (Left e)

mapErrors :: (Monad m) => (e -> e') -> ErrorfulT e m a -> ErrorfulT e' m a
mapErrors f m = ErrorfulT $ do
    x <- runErrorfulT m
    case x of
        Left  e      -> pure . Left  $ f e
        Right (a,es) -> pure . Right $ (a, f <$> es)

