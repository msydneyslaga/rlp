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
import Rlp.AltSyntax                    as Rlp
import Compiler.Types
import Data.Pretty
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

desugarRlpProgR :: forall m a. (Monad m)
                => Rlp.Program PsName a
                -> RLPCT m Core.Program'
desugarRlpProgR p = do
    let p' = desugarRlpProg p
    addDebugMsg "dump-desugared" $ show (out p')
    pure p'

desugarRlpProg = undefined

desugarRlpExpr = undefined

runNameSupply :: Text -> Eff (NameSupply ': es) a -> Eff es a
runNameSupply pre = undefined -- evalState [ pre <> "_" <> tshow name | name <- [0..] ]

-- the rl' program is desugared by desugaring each declaration as a separate
-- program, and taking the monoidal product of the lot :3

rlpProgToCore :: Rlp.Program PsName (RlpExpr PsName) -> Program'
rlpProgToCore = foldMapOf (programDecls . each) declToCore

declToCore :: Rlp.Decl PsName (RlpExpr PsName) -> Program'

-- assume all arguments are VarP's for now
declToCore (FunD b as e) = mempty & programScDefs .~ [ScDef b as' e']
    where
        as' = as ^.. each . singular _VarP
        e' = runPureEff . runNameSupply b . exprToCore $ e

type NameSupply = State [Name]

exprToCore :: (NameSupply :> es)
           => RlpExpr PsName -> Eff es Core.Expr'
exprToCore = foldFixM \case
    InL e -> pure $ Fix e
    InR e -> rlpExprToCore e

rlpExprToCore :: (NameSupply :> es)
           => Rlp.ExprF PsName Core.Expr' -> Eff es Core.Expr'

-- assume all binders are simple variable patterns for now
rlpExprToCore (LetEF r bs e) = pure $ Let r bs' e
  where
    bs' = b2b <$> bs
    b2b (VarB (VarP k) v) = Binding k v

