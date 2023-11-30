{-|
Module      : Compiler.RLPC
Description : Tools used to glue each piece of RLPC together

This module implements the toolset common to the entire compiler, most notably
errors and the family of RLPC monads.
-}
{-# LANGUAGE GeneralisedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia #-}
module Compiler.RLPC
    ( RLPC
    , RLPCT
    , RLPCIO
    , RLPCOptions(RLPCOptions)
    , addFatal
    , addWound
    , MonadErrorful
    , Severity(..)
    , Evaluator(..)
    , evalRLPCT
    , evalRLPCIO
    , evalRLPC
    , rlpcLogFile
    , rlpcDebugOpts
    , rlpcEvaluator
    , rlpcInputFiles
    , DebugFlag(..)
    , whenFlag
    , flagDDumpEval
    , flagDDumpOpts
    )

    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Monad.Reader
import Control.Monad.Errorful
import Data.Functor.Identity
import Data.Default.Class
import GHC.Generics             (Generic)
import Data.Hashable            (Hashable)
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import Data.Coerce
import Lens.Micro
import Lens.Micro.TH
----------------------------------------------------------------------------------

-- TODO: fancy errors
newtype RLPCT e m a = RLPCT {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT e m) a
    }
    deriving (Functor, Applicative, Monad, MonadReader RLPCOptions)

deriving instance (MonadIO m) => MonadIO (RLPCT e m)

type RLPC e = RLPCT e Identity

type RLPCIO e = RLPCT e IO

evalRLPCT :: RLPCOptions
          -> RLPCT e m a
          -> m (Either e (a, [e]))
evalRLPCT o = runRLPCT >>> flip runReaderT o >>> runErrorfulT

evalRLPC :: RLPCOptions
         -> RLPC e a
         -> Either e (a, [e])
evalRLPC o m = coerce $ evalRLPCT o m

evalRLPCIO :: RLPCOptions
           -> RLPCIO e a
           -> IO (a, [e])
evalRLPCIO o m = do
    m' <- evalRLPCT o m
    case m' of
        Left e -> error "need to impl io errors llol" -- TODO
        Right a -> pure a
    

data RLPCOptions = RLPCOptions
    { _rlpcLogFile    :: Maybe FilePath
    , _rlpcDebugOpts  :: DebugOpts
    , _rlpcEvaluator  :: Evaluator
    , _rlpcInputFiles :: [FilePath]
    }
    deriving Show

data Evaluator = EvaluatorGM | EvaluatorTI
    deriving Show

data Severity = Error
              | Warning
              | Debug
              deriving Show

-- temporary until we have a new doc building system
type ErrorDoc = String

class Diagnostic e where
    errorDoc :: e -> ErrorDoc

instance (Monad m) => MonadErrorful e (RLPCT e m) where
    addWound = RLPCT . lift . addWound
    addFatal = RLPCT . lift . addFatal

----------------------------------------------------------------------------------

instance Default RLPCOptions where
    def = RLPCOptions
        { _rlpcLogFile = Nothing
        , _rlpcDebugOpts = mempty
        , _rlpcEvaluator = EvaluatorGM
        , _rlpcInputFiles = []
        }

type DebugOpts = HashSet DebugFlag

data DebugFlag = DDumpEval
               | DDumpOpts
               deriving (Show, Eq, Generic)

    -- deriving (Hashable)
    -- via Generically DebugFlag

instance Hashable DebugFlag

makeLenses ''RLPCOptions
pure []

whenFlag :: (MonadReader s m) => SimpleGetter s Bool -> m () -> m ()
whenFlag l m = asks (^. l) >>= \a -> if a then m else pure ()

-- there's probably a better way to write this. my current knowledge of lenses
-- is too weak.
flagGetter :: DebugFlag -> SimpleGetter RLPCOptions Bool
flagGetter d = to $ \s -> s ^. rlpcDebugOpts & S.member d

flagDDumpEval :: SimpleGetter RLPCOptions Bool
flagDDumpEval = flagGetter DDumpEval

flagDDumpOpts :: SimpleGetter RLPCOptions Bool
flagDDumpOpts = flagGetter DDumpOpts

