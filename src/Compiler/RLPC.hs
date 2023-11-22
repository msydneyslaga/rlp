{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.RLPC
    ( RLPC(..)
    , RLPCOptions(RLPCOptions)
    , addFatal
    , addWound
    , Severity(..)
    , SrcError(..)
    , evalRLPCT
    , evalRLPC
    )
    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Monad.Reader
import Control.Monad.Errorful
import Data.Functor.Identity
import Data.Coerce
import Lens.Micro
import Lens.Micro.TH
----------------------------------------------------------------------------------

-- TODO: fancy errors
newtype RLPCT e m a = RLPC {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT (SrcError e) m) a
    }
    deriving (Functor, Applicative, Monad, MonadReader RLPCOptions)

type RLPC e = RLPCT e Identity

evalRLPCT :: RLPCOptions
          -> RLPCT e m a
          -> m (Either (SrcError e) (a, [SrcError e]))
evalRLPCT o = runRLPCT >>> flip runReaderT o >>> runErrorfulT

evalRLPC :: RLPCOptions
         -> RLPC e a
         -> Either (SrcError e) (a, [SrcError e])
evalRLPC o m = coerce $ evalRLPCT o m

data RLPCOptions = RLPCOptions

data SrcError e = SrcError
    { _errSpan       :: (Int, Int, Int)
    , _errSeverity   :: Severity
    , _errDiagnostic :: e
    }

deriving instance (Show e) => Show (SrcError e)

data Severity = Error
              | Warning
              | Debug
              deriving Show

-- temporary until we have a new doc building system
type ErrorDoc = String

class Diagnostic e where
    errorDoc :: e -> ErrorDoc

-- makeLenses ''RLPCOptions
makeLenses ''SrcError

pure []

instance MonadErrorful (SrcError e) (RLPC e) where
    addWound = RLPC . lift . addWound
    addFatal = RLPC . lift . addFatal

