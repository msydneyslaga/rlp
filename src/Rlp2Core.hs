{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
module Rlp2Core
    ( desugarRlpProgR
    , desugarRlpProg
    , desugarRlpExpr
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.Writer.CPS
import Control.Monad.Utils
import Control.Arrow
import Control.Applicative
import Control.Comonad
import Control.Lens
import Compiler.RLPC
import Data.List                        (mapAccumL, partition)
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H
import Data.Monoid                      (Endo(..))
import Data.Either                      (partitionEithers)
import Data.Foldable
import Data.Fix
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Functor.Bind
import Data.Function                    (on)
import GHC.Stack
import Debug.Trace
import Numeric

import Effectful.State.Static.Local
import Effectful.Labeled
import Effectful
import Text.Show.Deriving

import Core.Syntax                      as Core
import Rlp.Syntax                       as Rlp
import Compiler.Types
import Data.Pretty                      (render, pretty)
import Rlp.Parse.Types                  (RlpcPs, PsName)
--------------------------------------------------------------------------------

type Tree a = Either Name (Name, Branch a)

-- | Rose tree branch representing "nested" "patterns" in the Core language. That
-- is, a constructor with children that are either a normal binder (Left (Given)
-- name) or an indirection to another pattern (Right (Generated name) (Pattern))

data Branch a = Branch Name [Tree a]
    deriving (Show, Functor, Foldable, Traversable)

-- | The actual rose tree.
-- @type Rose = 'Data.Fix.Fix' 'Branch'@

type Rose = Fix Branch

deriveShow1 ''Branch

--------------------------------------------------------------------------------

desugarRlpProgR :: forall m. (Monad m)
                => Rlp.Program RlpcPs SrcSpan
                -> RLPCT m Core.Program'
desugarRlpProgR p = do
    let p' = desugarRlpProg p
    addDebugMsg "dump-desugared" $ render (pretty p')
    pure p'

desugarRlpProg :: Rlp.Program RlpcPs SrcSpan -> Core.Program'
desugarRlpProg = rlpProgToCore

desugarRlpExpr :: Rlp.Expr' RlpcPs SrcSpan -> Core.Expr'
desugarRlpExpr = runPureEff . runNameSupply "anon" . undefined

runNameSupply :: Text -> Eff (NameSupply ': es) a -> Eff es a
runNameSupply pre = undefined -- evalState [ pre <> "_" <> tshow name | name <- [0..] ]

-- the rl' program is desugared by desugaring each declaration as a separate
-- program, and taking the monoidal product of the lot :3

rlpProgToCore :: Rlp.Program RlpcPs SrcSpan -> Program'
rlpProgToCore = foldMapOf (programDecls . each) declToCore

declToCore :: Rlp.Decl RlpcPs SrcSpan -> Program'
declToCore = undefined

type NameSupply = State [Name]

exprToCore :: (NameSupply :> es)
           => Rlp.ExprF RlpcPs a -> Eff es Core.Expr'
exprToCore = undefined

