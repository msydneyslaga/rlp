{-|
Module      : Compiler.RLPC
Description : Tools used to glue each piece of RLPC together

This module implements the toolset common to the entire compiler, most notably
errors and the family of RLPC monads.
-}
{-# LANGUAGE GeneralisedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-- only used for mtl instances
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DeriveGeneric, DerivingStrategies, DerivingVia #-}
module Compiler.RLPC
    ( RLPC
    , RLPCT(..)
    , RLPCIO
    , RLPCOptions(RLPCOptions)
    , IsRlpcError(..)
    , RlpcError(..)
    , MsgEnvelope(..)
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
    , flagDDumpAST
    , def
    , liftErrorful
    )
    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Exception
import Control.Monad.Reader
import Control.Monad.State      (MonadState(state))
import Control.Monad.Errorful
import Compiler.RlpcError
import Data.Functor.Identity
import Data.Default.Class
import Data.Foldable
import GHC.Generics             (Generic)
import Data.Hashable            (Hashable)
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import Data.Coerce
import Lens.Micro
import Lens.Micro.TH
import System.Exit
----------------------------------------------------------------------------------

newtype RLPCT m a = RLPCT {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT (MsgEnvelope RlpcError) m) a
    }
    deriving (Functor, Applicative, Monad)

type RLPC = RLPCT Identity

type RLPCIO = RLPCT IO

evalRLPC :: RLPCOptions
         -> RLPC a
         -> (Maybe a, [MsgEnvelope RlpcError])
evalRLPC opt r = runRLPCT r
               & flip runReaderT opt
               & runErrorful

evalRLPCT :: (Monad m)
          => RLPCOptions
          -> RLPCT m a
          -> m (Maybe a, [MsgEnvelope RlpcError])
evalRLPCT = undefined

evalRLPCIO :: RLPCOptions -> RLPCIO a -> IO a
evalRLPCIO opt r = do
    (ma,es) <- evalRLPCT opt r
    putRlpcErrs es
    case ma of
        Just x  -> pure x
        Nothing -> die "Failed, no code compiled."

putRlpcErrs :: [MsgEnvelope RlpcError] -> IO ()
putRlpcErrs = traverse_ print

liftErrorful :: (Monad m, IsRlpcError e) => ErrorfulT (MsgEnvelope e) m a -> RLPCT m a
liftErrorful e = RLPCT $ lift (fmap liftRlpcError `mapErrorful` e)

data RLPCOptions = RLPCOptions
    { _rlpcLogFile     :: Maybe FilePath
    , _rlpcDebugOpts   :: DebugOpts
    , _rlpcEvaluator   :: Evaluator
    , _rlpcHeapTrigger :: Int
    , _rlpcInputFiles  :: [FilePath]
    }
    deriving Show

data Evaluator = EvaluatorGM | EvaluatorTI
    deriving Show

----------------------------------------------------------------------------------

instance Default RLPCOptions where
    def = RLPCOptions
        { _rlpcLogFile = Nothing
        , _rlpcDebugOpts = mempty
        , _rlpcEvaluator = EvaluatorGM
        , _rlpcHeapTrigger = 200
        , _rlpcInputFiles = []
        }

type DebugOpts = HashSet DebugFlag

data DebugFlag = DDumpEval
               | DDumpOpts
               | DDumpAST
               deriving (Show, Eq, Generic)

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

flagDDumpAST :: SimpleGetter RLPCOptions Bool
flagDDumpAST = flagGetter DDumpAST

