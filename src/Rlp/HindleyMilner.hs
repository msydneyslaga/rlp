{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Rlp.HindleyMilner
    ( typeCheckRlpProgR
    , solve
    , annotate
    , TypeError(..)
    , runHM'
    , HM
    )
    where
--------------------------------------------------------------------------------
import Control.Lens             hiding (Context', Context, (:<), para)
import Control.Lens.Unsound
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad.Accum
import Control.Monad
import Control.Arrow            ((>>>))
import Control.Monad.Writer.Strict
import Data.Text                qualified as T
import Data.Pretty              hiding (annotate)
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

fixCofree :: (Functor f, Functor g)
          => Iso (Fix f) (Fix g) (Cofree f ()) (Cofree g b)
fixCofree = iso sa bt where
    sa = foldFix (() :<)
    bt (_ :< as) = Fix $ bt <$> as

lookupVar :: PsName -> Context -> HM (Type PsName)
lookupVar n g = case g ^. contextVars . at n of
    Just t -> pure t
    Nothing -> addFatal $ TyErrUntypedVariable n

gather :: RlpExpr PsName -> HM (Type PsName, PartialJudgement)
gather e = look >>= (H.lookup e >>> maybe memoise pure)
    where
        memoise = do
            r <- gather' e
            add (H.singleton e r)
            pure r

gather' :: RlpExpr PsName -> HM (Type PsName, PartialJudgement)
gather' = \case
    Finl (LitF (IntL _)) -> pure (IntT, mempty)

    Finl (VarF n) -> do
        t <- freshTv
        let j = mempty & assumptions .~ H.singleton n [t]
        pure (t,j)

    Finl (AppF f x) -> do
        tfx <- freshTv
        (tf,jf) <- gather f
        (tx,jx) <- gather x
        let jtfx = mempty & constraints .~ [Equality tf (tx :-> tfx)]
        pure (tfx, jf <> jx <> jtfx)

    Finl (LamF bs e) -> do
        tbs <- for bs (const freshTv)
        (te,je) <- gather e
        let cs = concatMap (uncurry . equals $ je ^. assumptions) $ bs `zip` tbs
            as = foldr H.delete (je ^. assumptions) bs
            j = mempty & constraints .~ cs & assumptions .~ as
            t = foldr (:->) te tbs
        pure (t,j)
      where
        equals as b tb = maybe []
            (fmap $ Equality tb)
            (as ^. at b)

    -- Finl (LamF [b] e) -> do
    --     tb <- freshTv
    --     (te,je) <- gather e
    --     let cs = maybe [] (fmap $ Equality tb) (je ^. assumptions . at b)
    --         as = je ^. assumptions & at b .~ Nothing
    --         j = mempty & constraints .~ cs & assumptions .~ as
    --         t = tb :-> te
    --     pure (t,j)

unify :: [Constraint] -> HM Context

unify [] = pure mempty

unify (Equality (sx :-> sy) (tx :-> ty) : cs) =
    unify $ Equality sx tx : Equality sy ty : cs

-- elim
unify (Equality (ConT s) (ConT t) : cs) | s == t = unify cs
unify (Equality (VarT s) (VarT t) : cs) | s == t = unify cs

unify (Equality (VarT s) t : cs)
    | occurs s t = addFatal $ TyErrRecursiveType s t
    | otherwise  = unify cs' <&> contextVars . at s ?~ t
  where
    cs' = cs & each . constraintTypes %~ subst s t

-- swap
unify (Equality s (VarT t) : cs) = unify (Equality (VarT t) s : cs)

unify (Equality s t : _) = addFatal $ TyErrCouldNotUnify s t

annotate :: RlpExpr PsName
         -> HM (Cofree (RlpExprF PsName) (Type PsName, PartialJudgement))
annotate = sequenceA . fixtend (gather . wrapFix)

solveTree :: Cofree (RlpExprF PsName) (Type PsName, PartialJudgement)
          -> HM (Type PsName)
solveTree e = undefined

infer1 :: RlpExpr PsName -> HM (Type PsName)
infer1 e = do
    ((t,j) :< _) <- annotate e
    g <- unify (j ^. constraints)
    pure $ ifoldrOf (contextVars . itraversed) subst t g

solve :: RlpExpr PsName -> HM (Cofree (RlpExprF PsName) (Type PsName))
solve e = sequenceA $ fixtend (infer1 . wrapFix) e

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

prettyHM :: (Out a)
         => Either [TypeError] (a, [Constraint])
         -> Either [TypeError] (String, [String])
prettyHM = over (mapped . _1) rout
         . over (mapped . _2 . each) rout

fixtend :: Functor f => (f (Fix f) -> b) -> Fix f -> Cofree f b
fixtend c (Fix f) = c f :< fmap (fixtend c) f

infer :: RlpExpr PsName -> HM (Cofree (RlpExprF PsName) (Type PsName))
infer = undefined

typeCheckRlpProgR :: (Monad m)
                  => Program PsName (RlpExpr PsName)
                  -> RLPCT m (Program PsName
                      (Cofree (RlpExprF PsName) (Type PsName)))
typeCheckRlpProgR = undefined

