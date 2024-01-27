{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Compiler.RlpcError
    ( IsRlpcError(..)
    , MsgEnvelope(..)
    , Severity(..)
    , RlpcError(..)
    , SrcSpan(..)
    , msgSpan
    , msgDiagnostic
    , msgSeverity
    , liftRlpcErrors
    , errorMsg
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Errorful
import Data.Text                (Text)
import Data.Text                qualified as T
import GHC.Exts                 (IsString(..))
import Lens.Micro.Platform
import Lens.Micro.Platform.Internal
----------------------------------------------------------------------------------

data MsgEnvelope e = MsgEnvelope
   { _msgSpan        :: SrcSpan
   , _msgDiagnostic  :: e
   , _msgSeverity    :: Severity
   }
   deriving (Functor, Show)

newtype RlpcError = Text [Text]
    deriving Show

instance IsString RlpcError where
    fromString = Text . pure . T.pack

class IsRlpcError e where
    liftRlpcError :: e -> RlpcError

instance IsRlpcError RlpcError where
    liftRlpcError = id

data Severity = SevWarning
              | SevError
              deriving Show

data SrcSpan = SrcSpan
    !Int -- ^ Line
    !Int -- ^ Column
    !Int -- ^ Absolute
    !Int -- ^ Length
    deriving Show

makeLenses ''MsgEnvelope

liftRlpcErrors :: (Functor m, IsRlpcError e)
               => ErrorfulT e m a
               -> ErrorfulT RlpcError m a
liftRlpcErrors = mapErrorful liftRlpcError

instance (IsRlpcError e) => IsRlpcError (MsgEnvelope e) where
    liftRlpcError msg = msg ^. msgDiagnostic & liftRlpcError

errorMsg :: SrcSpan -> e -> MsgEnvelope e
errorMsg s e = MsgEnvelope
    { _msgSpan = s
    , _msgDiagnostic = e
    , _msgSeverity = SevError
    }

