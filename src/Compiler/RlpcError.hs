{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Compiler.RlpcError
    ( IsRlpcError(..)
    , MsgEnvelope(..)
    , Severity(..)
    , RlpcError(..)
    , msgSpan
    , msgDiagnostic
    , msgSeverity
    , liftRlpcErrors
    , errorMsg
    , debugMsg
    -- * Located Comonad
    , Located(..)
    , SrcSpan(..)
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Errorful
import Data.Text                (Text)
import Data.Text                qualified as T
import GHC.Exts                 (IsString(..))
import Lens.Micro.Platform
import Lens.Micro.Platform.Internal
import Compiler.Types
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
              | SevDebug Text
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

debugMsg :: Text -> e -> MsgEnvelope e
debugMsg tag e = MsgEnvelope
    -- TODO: not pretty, but it is a debug message after all
    { _msgSpan = SrcSpan 0 0 0 0
    , _msgDiagnostic = e
    , _msgSeverity = SevDebug tag
    }

