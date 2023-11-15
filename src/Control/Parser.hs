{-|
Module      : Control.Parser
Description : Parser combinators

This module implements an interface for parser *types*, used in lexical analysis
and parsing. For the implementation of the rlp language's parser, see 'Parse'.
-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
module Control.Parser
    ( ParserT
    , runParserT

    , satisfy
    , char
    , spaces
    , nl
    , surround
    , string

    -- * Control.Applicative re-exports
    , (<|>)
    , many
    , some
    )
    where
----------------------------------------------------------------------------------
import Control.Applicative
import Control.Arrow                ((***))
import Control.Monad
import Data.Char
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

instance (MonadFail m) => MonadFail (ParserT i m) where
    fail s = ParserT $ \i -> fail s
----------------------------------------------------------------------------------

-- TODO: generalise to non-lists
satisfy :: (MonadPlus m) => (a -> Bool) -> ParserT [a] m a
satisfy p = ParserT $ \case
    (x:xs) | p x -> pure (xs,x)
    _            -> empty


char :: (MonadPlus m, Eq a) => a -> ParserT [a] m a
char c = satisfy (==c)

string :: (MonadPlus m, Eq a) => [a] -> ParserT [a] m [a]
string s = sequenceA $ char <$> s

----------------------------------------------------------------------------------

surround :: (MonadPlus m)
         => ParserT i m l
         -> ParserT i m r
         -> ParserT i m c
         -> ParserT i m c
surround l r c = l *> c <* r

spaces :: (MonadPlus m) => ParserT String m Int
spaces = length <$> many (satisfy (==' '))

nl :: (MonadPlus m) => ParserT String m Int
nl = undefined

