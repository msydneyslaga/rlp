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
import Control.Lens                     hiding ((:<))
import Compiler.RLPC
import Data.List                        (mapAccumL, partition)
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Data.HashMap.Strict              qualified as H
import Data.Monoid                      (Endo(..))
import Data.Either                      (partitionEithers)
import Data.Foldable
import Data.Maybe                       (fromJust, fromMaybe)
import Data.Function                    (on)
import GHC.Stack
import Debug.Trace
import Numeric

import Data.Fix                         hiding (cata, para, cataM)
import Data.Functor.Bind
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Control.Comonad

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

-- desugarRlpProgR :: forall m a. (Monad m)
--                 => Rlp.Program PsName (TypedRlpExpr PsName)
--                 -> RLPCT m (Core.Program Var)
-- desugarRlpProgR p = do
--     let p' = desugarRlpProg p
--     addDebugMsg "dump-desugared" $ show (out p')
--     pure p'

desugarRlpProgR = undefined

desugarRlpProg :: Rlp.Program PsName (TypedRlpExpr PsName) -> Core.Program Var
desugarRlpProg = rlpProgToCore

desugarRlpExpr = undefined

type NameSupply = Labeled "NameSupply" (State [Name])

runNameSupply :: Text -> Eff (NameSupply ': es) a -> Eff es a
runNameSupply pre = runLabeled $ evalState [ pre <> "_" <> tshow name | name <- [0..] ]
    where tshow = T.pack . show

-- the rl' program is desugared by desugaring each declaration as a separate
-- program, and taking the monoidal product of the lot :3

rlpProgToCore :: Rlp.Program PsName (TypedRlpExpr PsName) -> Core.Program Var
rlpProgToCore = foldMapOf (programDecls . each) declToCore

declToCore :: Rlp.Decl PsName (TypedRlpExpr PsName) -> Core.Program Var

-- assume full eta-expansion for now
declToCore (FunD b [] e) = mempty & programScDefs .~ [ScDef b' [] undefined]
    where
        b' = MkVar b (typeToCore $ extract e)
        e' = runPureEff . runNameSupply b . exprToCore $ e

typeToCore :: Rlp.Type PsName -> Core.Type
typeToCore (VarT n) = TyVar n

exprToCore :: (NameSupply :> es)
           => TypedRlpExpr PsName
           -> Eff es (Cofree (Core.ExprF Var) Core.Type)
exprToCore = cataM \case
    t :<$ InL e -> pure $ t' :< annotateVar t' e
        where t' = typeToCore t
    -- InL e -> pure . annotateVars . Fix $ e
    -- InR e -> rlpExprToCore e

annotateVar :: Core.Type -> Core.ExprF PsName a -> Core.ExprF Var a

-- fixed points:
annotateVar _ (VarF n)   = VarF n
annotateVar _ (ConF t a) = ConF t a
annotateVar _ (AppF f x) = AppF f x
annotateVar _ (LitF l)   = LitF l
annotateVar _ (TypeF t)  = TypeF t

rlpExprToCore :: (NameSupply :> es)
              => Rlp.ExprF PsName Core.Expr' -> Eff es Core.Expr'

-- assume all binders are simple variable patterns for now
rlpExprToCore (LetEF r bs e) = pure $ Let r bs' e
  where
    bs' = b2b <$> bs
    b2b (VarB (VarP k) v) = Binding k v

