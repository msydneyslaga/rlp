{-# LANGUAGE TemplateHaskell #-}
module Compiler.RlpcError
    ( IsRlpcError(..)
    , MsgEnvelope(..)
    , Severity
    , RlpcError(..)
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

data MsgEnvelope e = MsgEnvelope
   { _msgSpan        :: SrcSpan
   , _msgDiagnostic  :: e
   , _msgSeverity    :: Severity
   }

class IsRlpcError e where
    liftRlpcError :: e -> RlpcError

data RlpcError

data Severity = SevWarning
              | SevError
              deriving Show

data SrcSpan = SrcSpan
    !Int -- ^ Line
    !Int -- ^ Column
    !Int -- ^ Length

makeLenses ''MsgEnvelope

