{-|
Module      : Control.Parser
Description : The parser *object*

This module implements an interface for parser *types*, used in lexical analysis
and parsing. For the implementation of the rlp language's parser, see 'Parse'.
-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module Control.Parser
    (
    )
    where
----------------------------------------------------------------------------------
import Control.Applicative
import Control.Arrow                ((***))
import Control.Monad
----------------------------------------------------------------------------------

newtype ParserT i m o = ParserT { runParserT :: i -> m (i, o) }
    deriving (Functor)

instance (Monad m) => Applicative (ParserT i m) where
    pure a = ParserT \i -> pure (i, a)

    m <*> k = ParserT \i -> do
        (i',f) <- runParserT m i
        fmap (id *** f) $ runParserT k i'

instance (MonadPlus m) => Alternative (ParserT i m) where
    empty = ParserT $ const empty

    ParserT m <|> ParserT k = ParserT $ \i ->
        m i <|> k i

instance (MonadPlus m) => MonadPlus (ParserT i m)

instance (Monad m) => Monad (ParserT i m) where
    m >>= k = ParserT $ \i -> do
        (i',a) <- runParserT m i
        runParserT (k a) i'

----------------------------------------------------------------------------------

-- TODO: generalise to non-lists
satisfy :: (MonadPlus m, Eq a) => a -> ParserT [a] m a
satisfy c = ParserT $ \case
    (x:xs) | x == c -> pure (xs,x)
    _               -> empty

