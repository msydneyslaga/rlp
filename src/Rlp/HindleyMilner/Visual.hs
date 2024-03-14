{-# LANGUAGE LexicalNegation #-}
module Rlp.HindleyMilner.Visual
    (
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import System.IO
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.Text.IO                     qualified as T
import Data.Pretty                      hiding (annotate)
import Data.String                      (IsString(..))
import Data.Foldable
import Misc.CofreeF
import Control.Exception

import Data.Functor.Foldable

import Data.Aeson

import Core.Syntax                      as Core
import Rlp.AltSyntax                    as Rlp
import Rlp.HindleyMilner

import Prelude hiding ((**))
--------------------------------------------------------------------------------

type AnnExpr = Cofree (RlpExprF PsName)

