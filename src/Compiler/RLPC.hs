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
{-# LANGUAGE BlockArguments, ViewPatterns #-}
module Compiler.RLPC
    (
    -- * Rlpc Monad transformer
      RLPCT(RLPCT),
    -- ** Special cases
      RLPC, RLPCIO
    -- ** Running
    , runRLPCT
    , evalRLPCT, evalRLPCIO, evalRLPC
    -- * Rlpc options
    , Language(..), Evaluator(..)
    , DebugFlag(..), CompilerFlag(..)
    -- ** Lenses
    , rlpcLogFile, rlpcDFlags, rlpcEvaluator, rlpcInputFiles, rlpcLanguage
    -- * Misc. MTL-style functions
    , liftErrorful, hoistRlpcT
    -- * Misc. Rlpc Monad -related types
    , RLPCOptions(RLPCOptions), IsRlpcError(..), RlpcError(..)
    , MsgEnvelope(..), Severity(..)
    , addDebugMsg
    , whenDFlag, whenFFlag
    -- * Convenient re-exports
    , addFatal, addWound, def
    )
    where
----------------------------------------------------------------------------------
import Control.Arrow            ((>>>))
import Control.Exception
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State      (MonadState(state))
import Control.Monad.Errorful
import Control.Monad.IO.Class
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
import Lens.Micro.Platform.Internal
import System.Exit
----------------------------------------------------------------------------------

newtype RLPCT m a = RLPCT {
        runRLPCT :: ReaderT RLPCOptions (ErrorfulT (MsgEnvelope RlpcError) m) a
    }
    deriving ( Functor, Applicative, Monad
             , MonadReader RLPCOptions, MonadErrorful (MsgEnvelope RlpcError))

rlpc :: (IsRlpcError e, Monad m)
     => (RLPCOptions -> (Maybe a, [MsgEnvelope e]))
     -> RLPCT m a
rlpc f = RLPCT . ReaderT $ \opt ->
    ErrorfulT . pure $ f opt & _2 . each . mapped %~ liftRlpcError

type RLPC = RLPCT Identity

type RLPCIO = RLPCT IO

instance (MonadIO m) => MonadIO (RLPCT m) where

evalRLPC :: RLPCOptions
         -> RLPC a
         -> (Maybe a, [MsgEnvelope RlpcError])
evalRLPC opt r = runRLPCT r
               & flip runReaderT opt
               & runErrorful

evalRLPCT :: RLPCOptions
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
putRlpcErrs = traverse_ (putStrLn . ('\n':) . prettyRlpcMsg)

prettyRlpcMsg :: MsgEnvelope RlpcError -> String
prettyRlpcMsg m@(view msgSeverity -> SevDebug) = prettyRlpcDebugMsg m
prettyRlpcMsg m                                = render $ docRlpcErr m

prettyRlpcDebugMsg :: MsgEnvelope RlpcError -> String
prettyRlpcDebugMsg (view msgDiagnostic -> Text ts) =
    T.unpack . foldMap (`T.snoc` '\n') $ ts

docRlpcErr :: MsgEnvelope RlpcError -> Doc
docRlpcErr msg = header
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

hoistRlpcT :: (forall a. m a -> n a)
         -> RLPCT m a -> RLPCT n a
hoistRlpcT f rma = RLPCT $ ReaderT $ \opt ->
    ErrorfulT $ f $ evalRLPCT opt rma

data RLPCOptions = RLPCOptions
    { _rlpcLogFile     :: Maybe FilePath
    , _rlpcDFlags      :: HashSet DebugFlag
    , _rlpcFFlags      :: HashSet CompilerFlag
    , _rlpcEvaluator   :: Evaluator
    , _rlpcHeapTrigger :: Int
    , _rlpcLanguage    :: Language
    , _rlpcInputFiles  :: [FilePath]
    }
    deriving Show

data Evaluator = EvaluatorGM | EvaluatorTI
    deriving Show

data Language = LanguageRlp | LanguageCore
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
        , _rlpcLanguage = LanguageRlp
        }

-- debug flags are passed with -dFLAG
type DebugFlag = String

type CompilerFlag = String

makeLenses ''RLPCOptions
pure []

addDebugMsg :: (Monad m, IsText e) => e -> RLPCT m ()
addDebugMsg e = addWound . debugMsg $ Text [e ^. unpacked . packed]

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

