{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Rlp.HindleyMilner
    ( typeCheckRlpProgR
    , solve
    , TypeError(..)
    , runHM'
    , HM
    )
    where
--------------------------------------------------------------------------------
import Control.Lens             hiding (Context', Context, (:<), para)
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad
import Control.Monad.Writer.Strict
import Data.Text                qualified as T
import Data.Pretty
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

import Compiler.RLPC
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
    Nothing -> addFatal $ TyErrUntypedVariable n

gather :: Context -> RlpExpr PsName -> HM (Type PsName)
gather g = \case

    Finl (LitF (IntL _)) -> pure IntT

    Finl (AppF f x) -> do
        tf <- gather g f
        tx <- gather g x
        tfx <- freshTv
        addConstraint $ Equality tf (tx :-> tfx)
        pure tfx

    Finl (VarF n) -> lookupVar n g

    Finl (LamF bs e) -> do
        tbs <- for bs $ \b -> (b,) <$> freshTv
        te <- gather (supplement tbs g) e
        pure $ foldrOf (each . _2) (:->) te tbs

unify :: Context -> [Constraint] -> HM Context

unify g [] = pure g

unify g (Equality (sx :-> sy) (tx :-> ty) : cs) =
    unify g $ Equality sx tx : Equality sy ty : cs

-- elim
unify g (Equality (ConT s) (ConT t) : cs) | s == t = unify g cs
unify g (Equality (VarT s) (VarT t) : cs) | s == t = unify g cs

unify g (Equality (VarT s) t : cs)
    | occurs s t = addFatal $ TyErrRecursiveType s t
    | otherwise  = unify g' cs'
  where
    g' = supplement [(s,t)] g
    cs' = cs & each . constraintTypes %~ subst s t

-- swap
unify g (Equality s (VarT t) : cs) = unify g (Equality (VarT t) s : cs)

unify _ (Equality s t : _) = addFatal $ TyErrCouldNotUnify s t

solve :: Context -> RlpExpr PsName -> HM (Type PsName)
solve g e = do
    (t,cs) <- listen $ gather g e
    g' <- unify g cs
    pure $ ifoldrOf (contextVars . itraversed) subst t g'

occurs :: PsName -> Type PsName -> Bool
occurs n = cata \case
    VarTF m | n == m -> True
    t                -> or t

subst :: PsName -> Type PsName -> Type PsName -> Type PsName
subst n t' = para \case
    VarTF m | n == m -> t'
    -- shadowing
    ForallTF x (pre,post) | x == n    -> ForallT x pre
                          | otherwise -> ForallT x post
    t -> embed $ t <&> view _2

prettyHM :: (Pretty a)
         => Either [TypeError] (a, [Constraint])
         -> Either [TypeError] (String, [String])
prettyHM = over (mapped . _1) rpretty
         . over (mapped . _2 . each) rpretty

fixtend :: (f (Fix f) -> b) -> Fix f -> Cofree f b
fixtend = undefined

infer :: RlpExpr PsName -> HM (Cofree (RlpExprF PsName) (Type PsName))
infer = _ . fixtend (solve _ . wrapFix)

typeCheckRlpProgR :: (Monad m)
                  => Program PsName (RlpExpr PsName)
                  -> RLPCT m (Program PsName
                      (Cofree (RlpExprF PsName) (Type PsName)))
typeCheckRlpProgR = undefined

