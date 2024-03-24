{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
module Rlp.HindleyMilner
    ( typeCheckRlpProgR
    , annotate
    , TypeError(..)
    , runHM'
    , liftHM
    , HM
    , prettyVars
    , prettyVars'
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
import Data.List
import Data.Monoid
import Data.Text                qualified as T
import Data.Foldable            (fold)
import Data.Function
import Data.Pretty              hiding (annotate)
import Data.Hashable
import Data.HashMap.Strict      (HashMap)
import Data.HashMap.Strict      qualified as H
import Data.HashSet             (HashSet)
import Data.HashSet.Lens
import Data.HashSet             qualified as S
import Data.Maybe               (fromMaybe)
import Data.Traversable
import GHC.Generics             (Generic, Generically(..))
import Debug.Trace

import Data.Functor             hiding (unzip)
import Data.Functor.Foldable    hiding (fold)
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
        let cs = bs `zip` tbs
               & concatMap (uncurry $ elimAssumptions (je ^. assumptions))
            as = foldr H.delete (je ^. assumptions) bs
            j = mempty & constraints .~ (je ^. constraints <> cs)
                       & assumptions .~ as
            t = foldr (:->) te tbs
        pure (t,j)

    Finr (LetEF NonRec [VarB (VarP x) y] e) -> do
        (ty,jy) <- gather y
        (te,je) <- gather e
        traceM $ "ty: " <> show ty
        traceM $ "jy: " <> show jy
        traceM $ "te: " <> show te
        traceM $ "je: " <> show je
        undefined

    -- Finl (LamF [b] e) -> do
    --     tb <- freshTv
    --     (te,je) <- gather e
    --     let cs = maybe [] (fmap $ Equality tb) (je ^. assumptions . at b)
    --         as = je ^. assumptions & at b .~ Nothing
    --         j = mempty & constraints .~ cs & assumptions .~ as
    --         t = tb :-> te
    --     pure (t,j)

generalise :: Context -> Type PsName -> Type PsName
generalise g t = ifoldr (\n _ s -> ForallT n s) t vs
    where
        vs = H.difference (freeVariables t ^. hashMap)
                          (g ^. contextTyVars)

instantiate :: Type PsName -> HM (Type PsName)
instantiate (ForallT x m) = do
    tv <- freshTv
    subst x tv <$> instantiate m
instantiate x = pure x

unify :: [Constraint] -> HM [(PsName, Type PsName)]

unify [] = pure mempty

unify (Equality (sx :-> sy) (tx :-> ty) : cs) =
    unify $ Equality sx tx : Equality sy ty : cs

-- elim
unify (Equality (ConT s) (ConT t) : cs) | s == t = unify cs
unify (Equality (VarT s) (VarT t) : cs) | s == t = unify cs

unify (Equality (VarT s) t : cs)
    | occurs s t = addFatal $ TyErrRecursiveType s t
    | otherwise  = unify cs' <&> ((s,t):)
  where
    cs' = cs & each . constraintTypes %~ subst s t

-- swap
unify (Equality s (VarT t) : cs) = unify (Equality (VarT t) s : cs)

-- failure!
unify (Equality s t : _) = addFatal $ TyErrCouldNotUnify s t

annotate :: RlpExpr PsName
         -> HM (Cofree (RlpExprF PsName) (Type PsName, PartialJudgement))
annotate = sequenceA . fixtend (gather . wrapFix)

assocs :: IndexedTraversal k [(k,v)] [(k,v')] v v'
assocs f []         = pure []
assocs f ((k,v):xs) = (\v' xs' -> (k,v') : xs')
                  <$> indexed f k v <*> assocs f xs

elimAssumptions :: Assumptions -> PsName -> Type PsName -> [Constraint]
-- elimAssumptions b tb as = maybe [] (fmap $ Equality tb) (as ^. at b)
elimAssumptions as b tb =
    as ^. at b . non' _Empty & each %~ Equality tb

elimAssumptionsG :: Context -> Assumptions -> [Constraint]
elimAssumptionsG g as =
    iconcatMapOf (contextVars . itraversed) (elimAssumptions as) g

infer :: Context -> RlpExpr PsName
      -> HM (Cofree (RlpExprF PsName) (Type PsName))
infer g0 e = do
    e' <- annotate e
    let (as, concat -> cs) = unzip $ e' ^.. folded . _2
                                          . lensProduct assumptions constraints
        cs' = concatMap (elimAssumptionsG g0) as <> cs
    g <- unify cs'
    let sub t = ifoldrOf (reversed . assocs) subst t g
    pure $ sub . view _1 <$> e'
  where
    -- intuitively, we'd return mgu(s,t) but the union is left-biased making `s`
    -- the user-specified type: prioritise her.
    unifyTypes _ s t = unify [Equality s t] $> s

infer1 :: Context -> RlpExpr PsName -> HM (Type PsName)
infer1 g = fmap extract . infer g

-- unionContextWithKeyM :: Monad m
--                      => (PsName -> Type PsName -> Type PsName
--                                                -> m (Type PsName))
--                      -> Context -> Context -> m Context
-- unionContextWithKeyM f a b = Context <$> unionWithKeyM f a' b'
--     where
--         a' = a ^. contextVars
--         b' = b ^. contextVars

-- unionWithKeyM :: forall m k v. (Eq k, Hashable k, Monad m)
--               => (k -> v -> v -> m v) -> HashMap k v -> HashMap k v
--               -> m (HashMap k v)
-- unionWithKeyM f a b = sequenceA $ H.unionWithKey f' ma mb
--     where
--         f' k x y = join $ liftA2 (f k) x y
--         ma = fmap (pure @m) a
--         mb = fmap (pure @m) b

-- solve :: RlpExpr PsName -> HM (Cofree (RlpExprF PsName) (Type PsName))
-- solve = solve' mempty

-- solve' :: Context -> RlpExpr PsName
--        -> HM (Cofree (RlpExprF PsName) (Type PsName))
-- solve' g = sequenceA . fixtend (infer1' g . wrapFix)

occurs :: PsName -> Type PsName -> Bool
occurs n = cata \case
    VarTF m | n == m -> True
    t                -> or t

subst :: PsName -> Type PsName -> Type PsName -> Type PsName
subst n t' = para \case
    VarTF m | n == m -> t'
    -- shadowing
    ForallTF x (pre,post) | x == n    -> ForallT x pre
    t -> embed $ t <&> view _2

prettyHM :: (Out a)
         => Either [TypeError] (a, [Constraint])
         -> Either [TypeError] (String, [String])
prettyHM = over (mapped . _1) rout
         . over (mapped . _2 . each) rout

fixtend :: Functor f => (f (Fix f) -> b) -> Fix f -> Cofree f b
fixtend c (Fix f) = c f :< fmap (fixtend c) f

buildInitialContext :: Program PsName a -> Context
buildInitialContext = const mempty
    -- Context . H.fromList . toListOf (programDecls . each . _TySigD)

typeCheckRlpProgR :: (Monad m)
                  => Program PsName (RlpExpr PsName)
                  -> RLPCT m (Program PsName
                      (TypedRlpExpr PsName))
typeCheckRlpProgR p = tc p
  where
    g = buildInitialContext p
    tc = liftHM . traverse (infer g) . etaExpandAll
    etaExpandAll = programDecls . each %~ etaExpand

etaExpand :: Decl b (RlpExpr b) -> Decl b (RlpExpr b)
etaExpand (FunD n [] e) = FunD n [] e
etaExpand (FunD n as e)
    | Right as' <- allVarP as
    = FunD n [] (Finl . LamF as' $ e)
  where
    allVarP = traverse (matching _VarP)
etaExpand a = a

liftHM :: (Monad m) => HM a -> RLPCT m a
liftHM = liftEither . runHM'

freeVariables :: Type PsName -> HashSet PsName
freeVariables = cata \case
    VarTF x -> S.singleton x
    ForallTF x m -> S.delete x m
    vs -> fold vs

boundVariables :: Type PsName -> HashSet PsName
boundVariables = cata \case
    ForallTF x m -> S.singleton x <> m
    vs -> fold vs

-- | rename all free variables for aesthetic purposes

prettyVars' :: Type PsName -> Type PsName
prettyVars' = join prettyVars

freeVariablesLTR :: Type PsName -> [PsName]
freeVariablesLTR = nub . cata \case
    VarTF x -> [x]
    ForallTF x m -> m \\ [x]
    vs -> concat vs

-- | for some type, compute a substitution which will rename all free variables
-- for aesthetic purposes

prettyVars :: Type PsName -> Type PsName -> Type PsName
prettyVars root = appEndo (foldMap Endo subs)
    where
        alphabetNames = [ T.pack [c] | c <- ['a'..'z'] ]
        names = alphabetNames \\ S.toList (boundVariables root)
        subs = zipWith (\k v -> subst k (VarT v))
                       (freeVariablesLTR root)
                       names

