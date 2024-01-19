{-# LANGUAGE TemplateHaskell #-}
module Compiler.RlpcError
    ( RlpcError(..)
    , MsgEnvelope(..)
    , Severity
    , RlpcErrorDoc(..)
    , SrcSpan(..)
    , msgSpan
    , msgDiagnostic
    , msgSeverity
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.Errorful
import Lens.Micro.TH
----------------------------------------------------------------------------------

data MsgEnvelope = MsgEnvelope
   { _msgSpan        :: SrcSpan
   , _msgDiagnostic  :: forall e. (RlpcError e) => e
   , _msgSeverity    :: Severity
   }

class RlpcError e where
    liftRlpcError :: e -> RlpcErrorDoc

data RlpcErrorDoc

data Severity = SevWarning
              | SevError
              deriving Show

data SrcSpan = SrcSpan
    !Int -- ^ Line
    !Int -- ^ Column
    !Int -- ^ Length

makeLenses ''MsgEnvelope

