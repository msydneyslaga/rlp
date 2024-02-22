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
{-# LANGUAGE BlockArguments, ViewPatterns #-}
module Compiler.RLPC
    (
    -- * Rlpc Monad transformer
      RLPCT(RLPCT),
    -- ** Special cases
      RLPC, RLPCIO
    , liftIO
    -- ** Running
    , runRLPCT
    , evalRLPCT, evalRLPCIO, evalRLPC
    -- * Rlpc options
    , Language(..), Evaluator(..)
    , DebugFlag(..), CompilerFlag(..)
    -- ** Lenses
    , rlpcLogFile, rlpcDFlags, rlpcEvaluator, rlpcInputFiles, rlpcLanguage
    -- * Misc. MTL-style functions
    , liftErrorful, liftMaybe, hoistRlpcT
    -- * Misc. Rlpc Monad -related types
    , RLPCOptions(RLPCOptions), IsRlpcError(..), RlpcError(..)
    , MsgEnvelope(..), Severity(..)
    , addDebugMsg
    , whenDFlag, whenFFlag
    -- * Misc. Utilities
    , forFiles_, withSource
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
import Data.Text.IO             qualified as T
import System.IO
import Text.ANSI                qualified as Ansi
import Text.PrettyPrint         hiding ((<>))
import Control.Lens
import Data.Text.Lens           (packed, unpacked, IsText)
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

instance MonadTrans RLPCT where
    lift = RLPCT . lift . lift

instance (MonadIO m) => MonadIO (RLPCT m) where
    liftIO = lift . liftIO

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

liftErrorful :: (Monad m, IsRlpcError e) => ErrorfulT (MsgEnvelope e) m a -> RLPCT m a
liftErrorful e = RLPCT $ lift (fmap liftRlpcError `mapErrorful` e)

liftMaybe :: (Monad m) => Maybe a -> RLPCT m a
liftMaybe m = RLPCT . lift . ErrorfulT . pure $ (m, [])

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
    , _rlpcLanguage    :: Maybe Language
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
        , _rlpcLanguage = Nothing
        }

-- debug flags are passed with -dFLAG
type DebugFlag = Text

type CompilerFlag = Text

makeLenses ''RLPCOptions
pure []

addDebugMsg :: (Monad m, IsText e) => Text -> e -> RLPCT m ()
addDebugMsg tag e = addWound . debugMsg tag $ Text [e ^. unpacked . packed]

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

--------------------------------------------------------------------------------

evalRLPCIO :: RLPCOptions -> RLPCIO a -> IO a
evalRLPCIO opt r = do
    (ma,es) <- evalRLPCT opt r
    putRlpcErrs opt es
    case ma of
        Just x  -> pure x
        Nothing -> die "Failed, no code compiled."

putRlpcErrs :: RLPCOptions -> [MsgEnvelope RlpcError] -> IO ()
putRlpcErrs opt es = case opt ^. rlpcLogFile of
    Just lf -> withFile lf WriteMode putter
    Nothing -> putter stderr
  where
    putter h = hPutStrLn h `traverse_` renderRlpcErrs opt es

renderRlpcErrs :: RLPCOptions -> [MsgEnvelope RlpcError] -> [String]
renderRlpcErrs opts = (if don'tBother then id else filter byTag)
                  >>> fmap prettyRlpcMsg
    where
        dflags = opts ^. rlpcDFlags
        don'tBother = "ALL" `S.member` (opts ^. rlpcDFlags)

        byTag :: MsgEnvelope RlpcError -> Bool
        byTag (view msgSeverity -> SevDebug t) =
            t `S.member` dflags
        byTag _ = True

prettyRlpcMsg :: MsgEnvelope RlpcError -> String
prettyRlpcMsg m@(view msgSeverity -> SevDebug _) = prettyRlpcDebugMsg m
prettyRlpcMsg m                                  = render $ docRlpcErr m

prettyRlpcDebugMsg :: MsgEnvelope RlpcError -> String
prettyRlpcDebugMsg msg =
        T.unpack . foldMap mkLine $ [ t' | t <- ts, t' <- T.lines t ]
    where
        mkLine s = "-d" <> tag <> ": " <> s <> "\n"
        Text ts = msg ^. msgDiagnostic
        SevDebug tag = msg ^. msgSeverity

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
        pos = msgColour $ tshow (msg ^. msgSpan . srcSpanLine)
                       <> ":"
                       <> tshow (msg ^. msgSpan . srcSpanColumn)

        header = ttext $ filename <> msgColour ":" <> pos <> msgColour ": "
                        <> errorColour "error" <> msgColour ":"

        bullets = let Text ts = msg ^. msgDiagnostic
                  in vcat $ hang "•" 2 . ttext . msgColour <$> ts

        msgColour = Ansi.white . Ansi.bold
        errorColour = Ansi.red . Ansi.bold
        ttext = text . T.unpack
        tshow :: (Show a) => a -> Text
        tshow = T.pack . show

--------------------------------------------------------------------------------

forFiles_ :: (Monad m)
          => (FilePath -> RLPCT m a)
          -> RLPCT m ()
forFiles_ k = do
    fs <- view rlpcInputFiles
    forM_ fs k

-- TODO: catch any exceptions, i.e. non-existent files should be handled by the
-- compiler
withSource :: (MonadIO m) => FilePath -> (Text -> RLPCT m a) -> RLPCT m a
withSource f k = liftIO (T.readFile f) >>= k

