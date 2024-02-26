{-# LANGUAGE TemplateHaskell #-}
module Core.Parse.Types
    ( P(..)
    , psTyVars
    , def
    , PsName
    , finishTyping
    )
    where
--------------------------------------------------------------------------------
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Default
import Data.Tuple               (swap)

import Control.Lens

import Core.Syntax
--------------------------------------------------------------------------------

newtype P a = P { runP :: PState -> (PState, Maybe a) }
    deriving Functor

data PState = PState
    { _psTyVars :: [(Name, Kind)]
    }

instance Applicative P where
    pure a = P (, Just a)

    P pf <*> P pa = P \st ->
        let (st',mf) = pf st
            (st'',ma) = pa st'
        in (st'', mf <*> ma)

instance Monad P where
    P pa >>= k = P \st ->
        let (st',ma) = pa st
        in case ma of
            Just a -> runP (k a) st'
            Nothing -> (st', Nothing)

instance MonadState PState P where
    state = P . fmap ((_2 %~ Just) . review swapped)

instance Default PState where
    def = undefined

makeLenses ''PState

type PsName = Either Name Var

--------------------------------------------------------------------------------

finishTyping :: Program PsName -> P (Program Var)
finishTyping = error . show

