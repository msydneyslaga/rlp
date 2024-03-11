{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Rlp.HindleyMilner
    -- ( infer
    -- , check
    -- , TypeError(..)
    -- , HMError
    -- )
    where
--------------------------------------------------------------------------------
import Control.Lens             hiding (Context', Context, (:<), para)
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Text                qualified as T
import Data.Hashable
import Data.HashMap.Strict      (HashMap)
import Data.HashMap.Strict      qualified as H
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import Data.Maybe               (fromMaybe)
import Data.Traversable
import GHC.Generics             (Generic(..), Generically(..))

import Data.Functor
import Data.Functor.Foldable
import Data.Fix                 hiding (cata, para)
import Control.Comonad.Cofree
import Control.Comonad

import Compiler.RlpcError
import Rlp.AltSyntax            as Rlp
import Core.Syntax              qualified as Core
import Core.Syntax              (ExprF(..), Lit(..))
import Rlp.HindleyMilner.Types
--------------------------------------------------------------------------------

-- | Synonym for @Errorful [TypeError]@. This means an @HMError@ action may
-- throw any number of fatal or nonfatal errors. Run with @runErrorful@.
type HMError = Errorful TypeError

fixCofree :: (Functor f, Functor g)
          => Iso (Fix f) (Fix g) (Cofree f ()) (Cofree g b)
fixCofree = iso sa bt where
    sa = foldFix (() :<)
    bt (_ :< as) = Fix $ bt <$> as

lookupVar :: PsName -> Context -> HM (Type PsName)
lookupVar n g = case g ^. contextVars . at n of
    Just t -> pure t
    Nothing -> addFatal (TyErrUntypedVariable n)

-- | Instantiate a polytype by replacing the bound type variables with fresh
-- monotype (free) variables
inst :: Type PsName -> HM (Type PsName)
inst = para \case
    ForallTF x (_,t) -> do
        m <- t
        tv <- freshTv
        pure $ subst x tv m
    -- discard the recursive results by selected fst
    t -> pure . embed . fmap fst $ t

generalise :: Type PsName -> Type PsName
generalise = foldr ForallT <*> toListOf tyVars

tyVars :: Traversal (Type b) (Type b') b b'
tyVars = traverse

polytypeBinds :: Traversal' (Type b) b
polytypeBinds k (ForallT x m) = ForallT <$> k x <*> polytypeBinds k m
polytypeBinds k t             = pure t

subst :: PsName -> Type PsName -> Type PsName -> Type PsName
subst n t' = para \case
    VarTF m               | n == m    -> t'
    ForallTF m (pre,post) | n == m    -> pre
                          | otherwise -> post
    t -> embed . fmap snd $ t

occurs :: PsName -> Type PsName -> Bool
occurs n = cata \case
    VarTF m | n == m -> True
    t                -> or t

infer :: Context -> RlpExpr PsName -> HM (Type PsName)
infer g = \case

    Finl (LitF (IntL _)) -> pure IntT
    
    {- Var
     -   x : τ ∈ Γ
     -   τ' = inst τ
     -   -----------
     -   Γ |- x : τ'
     -}
    Finl (VarF x) -> do
        t <- lookupVar x g
        let t' = inst t
        t'

    Finl (AppF f x) -> do
        te <- infer g f
        tx <- infer g x
        t' <- freshTv
        undefined

unify :: Context -> Type PsName -> Type PsName -> Context
unify g = \cases
    IntT IntT -> g
    (VarT a) b | Just a' <- g ^. contextTyVars . at a -> unify g a' b
    b (VarT a) | Just a' <- g ^. contextTyVars . at a -> unify g b a'

    s@(VarT a) b | Nothing <- g ^. contextTyVars . at a
                 | s == b

