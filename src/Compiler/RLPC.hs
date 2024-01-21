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
import GHC.Generics             (Generic)
import Data.Hashable            (Hashable)
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import Data.Coerce
import Lens.Micro
import Lens.Micro.TH
----------------------------------------------------------------------------------

newtype RLPCT m a = RLPCT {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT RlpcError m) a
    }

type RLPC = RLPCT Identity

type RLPCIO = RLPCT IO

instance Functor (RLPCT m) where
instance Applicative (RLPCT m) where
instance Monad (RLPCT m) where

evalRLPC = undefined
evalRLPCT = undefined
evalRLPCIO = undefined

liftErrorful :: ErrorfulT e m a -> RLPCT m a
liftErrorful e = undefined

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

