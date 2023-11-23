{-# LANGUAGE GeneralisedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Compiler.RLPC
    ( RLPC(..)
    , RLPCIO
    , RLPCOptions(RLPCOptions)
    , addFatal
    , addWound
    , Severity(..)
    , SrcError(..)
    , evalRLPCT
    , evalRLPCIO
    , evalRLPC
    , rlpcLogFile   
    , rlpcDumpEval  
    , rlpcInputFiles
    )

    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Monad.Reader
import Control.Monad.Errorful
import Data.Functor.Identity
import Data.Default.Class
import Data.Coerce
import Lens.Micro
import Lens.Micro.TH
----------------------------------------------------------------------------------

-- TODO: fancy errors
newtype RLPCT e m a = RLPC {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT (SrcError e) m) a
    }
    deriving (Functor, Applicative, Monad, MonadReader RLPCOptions)

deriving instance (MonadIO m) => MonadIO (RLPCT e m)

type RLPC e = RLPCT e Identity

type RLPCIO e = RLPCT e IO

evalRLPCT :: RLPCOptions
          -> RLPCT e m a
          -> m (Either (SrcError e) (a, [SrcError e]))
evalRLPCT o = runRLPCT >>> flip runReaderT o >>> runErrorfulT

evalRLPC :: RLPCOptions
         -> RLPC e a
         -> Either (SrcError e) (a, [SrcError e])
evalRLPC o m = coerce $ evalRLPCT o m

evalRLPCIO :: RLPCOptions
           -> RLPCIO e a
           -> IO (a, [SrcError e])
evalRLPCIO o m = do
    m' <- evalRLPCT o m
    case m' of
        Left e -> error "need to impl io errors llol" -- TODO
        Right a -> pure a
    

data RLPCOptions = RLPCOptions
    { _rlpcLogFile    :: Maybe FilePath
    , _rlpcDumpEval   :: Bool
    , _rlpcInputFiles :: [FilePath]
    }
    deriving Show

instance Default RLPCOptions where
    def = RLPCOptions
        { _rlpcLogFile = Nothing
        , _rlpcDumpEval = False
        , _rlpcInputFiles = []
        }

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

makeLenses ''RLPCOptions
makeLenses ''SrcError

pure []

instance MonadErrorful (SrcError e) (RLPC e) where
    addWound = RLPC . lift . addWound
    addFatal = RLPC . lift . addFatal

