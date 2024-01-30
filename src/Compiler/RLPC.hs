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
    , rlpcDFlags  
    , rlpcEvaluator
    , rlpcInputFiles
    , DebugFlag(..)
    , whenDFlag
    , whenFFlag
    , def
    , liftErrorful
    )
    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State      (MonadState(state))
import Control.Monad.Errorful
import Compiler.RlpcError
import Compiler.Types
import Data.Functor.Identity
import Data.Default.Class
import Data.Foldable
import GHC.Generics             (Generic)
import Data.Maybe
import Data.Hashable            (Hashable)
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import Data.Coerce
import Data.Text                (Text)
import Data.Text                qualified as T
import Text.ANSI                qualified as Ansi
import Text.PrettyPrint         hiding ((<>))
import Lens.Micro.Platform
import System.Exit
----------------------------------------------------------------------------------

newtype RLPCT m a = RLPCT {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT (MsgEnvelope RlpcError) m) a
    }
    deriving (Functor, Applicative, Monad, MonadReader RLPCOptions)

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
evalRLPCT opt r = runRLPCT r
                & flip runReaderT opt
                & runErrorfulT

evalRLPCIO :: RLPCOptions -> RLPCIO a -> IO a
evalRLPCIO opt r = do
    (ma,es) <- evalRLPCT opt r
    putRlpcErrs es
    case ma of
        Just x  -> pure x
        Nothing -> die "Failed, no code compiled."

putRlpcErrs :: [MsgEnvelope RlpcError] -> IO ()
putRlpcErrs = traverse_ (putStrLn . ('\n':) . render . prettyRlpcErr)

prettyRlpcErr :: MsgEnvelope RlpcError -> Doc
prettyRlpcErr msg = header
                 $$ nest 2 bullets
                 $$ source
    where
        source = vcat $ zipWith (<+>) rule srclines
            where
                rule = repeat (ttext . Ansi.blue . Ansi.bold $ "|")
                srclines = ["", "<problematic source code>", ""]
        filename = msgColour "<input>"
        pos = msgColour $ tshow (msg ^. msgSpan . srcspanLine)
                       <> ":"
                       <> tshow (msg ^. msgSpan . srcspanColumn)

        header = ttext $ filename <> msgColour ":" <> pos <> msgColour ": "
                        <> errorColour "error" <> msgColour ":"

        bullets = let Text ts = msg ^. msgDiagnostic
                  in vcat $ hang "•" 2 . ttext . msgColour <$> ts

        msgColour = Ansi.white . Ansi.bold
        errorColour = Ansi.red . Ansi.bold
        ttext = text . T.unpack
        tshow :: (Show a) => a -> Text
        tshow = T.pack . show

liftErrorful :: (Monad m, IsRlpcError e) => ErrorfulT (MsgEnvelope e) m a -> RLPCT m a
liftErrorful e = RLPCT $ lift (fmap liftRlpcError `mapErrorful` e)

data RLPCOptions = RLPCOptions
    { _rlpcLogFile     :: Maybe FilePath
    , _rlpcDFlags      :: HashSet DebugFlag
    , _rlpcFFlags      :: HashSet CompilerFlag
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
        , _rlpcDFlags = mempty
        , _rlpcFFlags = mempty
        , _rlpcEvaluator = EvaluatorGM
        , _rlpcHeapTrigger = 200
        , _rlpcInputFiles = []
        }

-- debug flags are passed with -dFLAG
type DebugFlag = String

type CompilerFlag = String

makeLenses ''RLPCOptions
pure []

-- TODO: rewrite this with prisms once microlens-pro drops :3
whenDFlag :: (Monad m) => DebugFlag -> RLPCT m () -> RLPCT m ()
whenDFlag f m = do
    -- mfw no `At` instance for HashSet
    fs <- view rlpcDFlags
    let a = S.member f fs
    when a m

whenFFlag :: (Monad m) => CompilerFlag -> RLPCT m () -> RLPCT m ()
whenFFlag f m = do
    -- mfw no `At` instance for HashSet
    fs <- view rlpcFFlags
    let a = S.member f fs
    when a m

