{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
module Rlp.HindleyMilner
    ( typeCheckRlpProgR
    , annotate
    , TypeError(..)
    , runHM'
    , liftHM
    , HM
    , renamePrettily
    )
    where
--------------------------------------------------------------------------------
import Control.Lens             hiding (Context', Context, (:<), para, uncons)
import Control.Lens.Unsound
import Control.Lens.Extras
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad.Accum
import Control.Monad.Reader
import Control.Monad
import Control.Monad.Extra
import Control.Arrow            ((>>>))
import Control.Monad.Writer.Strict

import Data.List
import Data.Monoid
import Data.Text                qualified as T
import Data.Foldable            (fold)
import Data.Function
import Data.Foldable
import Data.Pretty              hiding (annotate)
import Data.Maybe
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
import Data.Functor.Extend
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
gather e = use hmMemo >>= (H.lookup e >>> maybe memoise pure)
    where
        memoise = do
            r <- gather' e
            hmMemo <>= H.singleton e r
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
        cs <- bs `zip` tbs
            & concatMapM (uncurry $ elimAssumptions (je ^. assumptions))
        let as = foldr H.delete (je ^. assumptions) bs
            j = mempty & constraints .~ (je ^. constraints <> cs)
                       & assumptions .~ as
            t = foldr (:->) te tbs
        pure (t,j)

    Finr (LetEF NonRec bs e) -> do
        let ks = bs ^.. each . singular _VarB . _1 . singular _VarP
        (txs,jxs) <- unzip <$> generaliseGatherBinds bs
        (te,je) <- gather e
        (cs,m) <- fmap fold . for (ks `zip` txs) $ \ (k,t) ->
            elimAssumptionsMap (je ^. assumptions) k t
        let jxcs = jxs ^. each . constraints
                 & each . constraintTypes %~ substMap m
            as = foldr H.delete (je ^. assumptions) ks
            j = mempty & constraints .~ je ^. constraints <> jxcs <> cs
                       & assumptions .~ foldOf (each . assumptions) jxs <> as
        pure (te, j)

    Finr (LetEF Rec bs e) -> do
        let ks = bs ^.. each . singular _VarB . _1 . singular _VarP
        (txs,txs',jxs) <- unzip3 <$> gatherBinds bs
        let jxsa = foldOf (each . assumptions) jxs
        jxcs <- elimWithBinds (ks `zip` txs) jxsa
        (te,je) <- gather e
        -- ... why don't we need the map?
        (cs,_) <- fmap fold . for (ks `zip` txs') $ \ (k,t) ->
            elimAssumptionsMap (je ^. assumptions) k t
        let as = deleteKeys ks (je ^. assumptions <> jxsa)
            j = mempty & constraints .~ je ^. constraints <> jxcs <> cs
                       & assumptions .~ as
        pure (te,j)

    -- Finr (LetEF Rec [VarB (VarP k) x] e) -> do
    --     ((tx,jx),frees) <- listenFreshTvNames $ gather x
    --     jxcs <- elimAssumptions' (jx ^. assumptions) k tx
    --     let tx' = generalise frees tx
    --     (te,je) <- gather e
    --     (cs,m) <- elimAssumptionsMap (je ^. assumptions) k tx'
    --     let as = H.delete k (je ^. assumptions)
    --           <> H.delete k (jx ^. assumptions)
    --         j = mempty & constraints .~ je ^. constraints <> jxcs <> cs
    --                    & assumptions .~ as
    --     pure (te,j)

deleteKeys :: (Eq k, Hashable k) => [k] -> HashMap k v -> HashMap k v
deleteKeys ks h = foldr H.delete h ks

gatherBinds :: [Binding PsName (RlpExpr PsName)]
            -> HM [( Type PsName -- inferred type
                   , Type PsName -- generalised type
                   , PartialJudgement )]
gatherBinds bs = for bs $ \ (VarB (VarP k) x) -> do
    ((tx,jx),frees) <- listenFreshTvNames $ gather x
    let tx' = generalise frees tx
    pure (tx,tx',jx)

generaliseGatherBinds :: [Binding PsName (RlpExpr PsName)]
                      -> HM [(Type PsName, PartialJudgement)]
generaliseGatherBinds = traverse \b ->
    b ^. singular _VarB . _2 & generaliseGather

generaliseGather :: RlpExpr PsName -> HM (Type PsName, PartialJudgement)
generaliseGather e = do
    (a,frees) <- listenFreshTvNames $ gather e
    pure $ a & _1 %~ generalise frees

generalise :: [PsName] -> Type PsName -> Type PsName
generalise freeTvs t = foldr ForallT t freeTvs

generaliseG :: Context -> Type PsName -> Type PsName
generaliseG g t = ifoldr (\n _ s -> ForallT n s) t vs where
    vs = H.difference (freeVariables t ^. hashMap)
                      (g ^. contextTyVars)

instantiate :: Type PsName -> HM (Type PsName)
instantiate (ForallT x m) = do
    tv <- freshTv
    subst x tv <$> instantiate m
instantiate x = pure x

instantiateMap :: Type PsName -> HM (Type PsName, HashMap PsName (Type PsName))
instantiateMap (ForallT x m) = do
    tv <- freshTv
    instantiateMap m & mapped . _2 %~ H.insert x tv
                     & mapped . _1 %~ subst x tv
instantiateMap t = pure (t, mempty)

saturated :: Type PsName -> HM [Type PsName]
saturated (ConT con `AppT` as) = do
    mk <- view $ contextTyCons . at con
    case mk of
      Nothing -> addFatal $ TyErrUntypedVariable con
      Just k | lengthOf arrowStops k - 1 == lengthOf applicants1 as
             -> pure (as ^.. applicants1)
             | otherwise
             -> undefined

unify :: [Constraint] -> HM [(PsName, Type PsName)]

unify [] = pure mempty

unify (Equality (sx :-> sy) (tx :-> ty) : cs) =
    unify $ Equality sx tx : Equality sy ty : cs

-- unify (Equality a@(ConT ca `AppT` as) b@(ConT cb `AppT` bs) : cs)
--   | ca == cb = do
--     cs' <- liftA2 (zipWith Equality) (saturated a) (saturated b)
--     unify $ cs' ++ cs

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

-- | @elimAssumptions as b tb@ eliminates each assumption in @as@ involving @b@
-- by translating the assumptions into constraints equating @b@'s assumed type
-- with @tb@

elimAssumptions :: Assumptions -> PsName -> Type PsName -> HM [Constraint]
elimAssumptions as b tb =
    as ^. at b . non' _Empty & traverseOf each k
  where k t = Equality tb <$> instantiate t

elimAssumptions' :: Assumptions -> PsName -> Type PsName -> HM [Constraint]
elimAssumptions' as b tb =
    as ^. at b . non' _Empty & traverseOf each k
  where k t = Equality <$> instantiate tb <*> instantiate t

elimAssumptionsMap :: Assumptions -> PsName -> Type PsName
                   -> HM ([Constraint], HashMap PsName (Type PsName))
elimAssumptionsMap as b tb =
    runWriterT $ as ^. at b . non' _Empty & traverseOf each k
  where
    k t = do
        (tb',w) <- lift $ instantiateMap tb
        (t',w') <- lift $ instantiateMap t
        writer (Equality tb' t', w <> w')

substMap :: HashMap PsName (Type PsName) -> Type PsName -> Type PsName
substMap m t = ifoldr subst t m

elimAssumptionsG :: Context -> Assumptions -> HM [Constraint]
elimAssumptionsG g as
    = g ^. contextVars
    & itraverse (elimAssumptions' as)
    & fmap (H.elems >>> concat)

finalJudgement :: Cofree (RlpExprF PsName) (Type PsName, PartialJudgement)
               -> PartialJudgement
finalJudgement = foldOf (folded . _2)

infer :: RlpExpr PsName -> HM (Cofree (RlpExprF PsName) (Type PsName))
infer e = do
    g0 <- ask
    e' <- annotate e
    let (cs,as) = finalJudgement e' ^. lensProduct constraints assumptions
    cs' <- (<>cs) <$> elimAssumptionsG g0 as
    -- checkUndefinedVariables e'
    sub <- solve cs'
    pure $ e' & fmap (sub . view _1)
              & _extract %~ generaliseG g0
  where
    -- intuitively, we'd return mgu(s,t) but the union is left-biased making `s`
    -- the user-specified type: prioritise her.
    unifyTypes _ s t = unify [Equality s t] $> s

solve :: [Constraint] -> HM (Type PsName -> Type PsName)
solve cs = do
    g <- unify cs
    pure $ \t -> ifoldrOf (reversed . assocs) subst t g

checkUndefinedVariables
    :: Cofree (RlpExprF PsName) (Type PsName, PartialJudgement)
    -> HM ()
checkUndefinedVariables ((_,j) :< es)
    = case j ^. assumptions of
        [] -> checkUndefinedVariables `traverse_` es
        as -> doErrs *> checkUndefinedVariables `traverse_` es
          where doErrs = ifor as \n _ -> addWound $ TyErrUntypedVariable n

infer1 :: RlpExpr PsName -> HM (Type PsName)
infer1 = fmap extract . infer

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
buildInitialContext = foldMapOf (programDecls . each) \case
    TySigD n t    -> contextOfTySig n t
    DataD n as cs -> contextOfData n as cs
    _             -> mempty

contextOfTySig :: PsName -> Type PsName -> Context
contextOfTySig = const $ const mempty

contextOfData :: PsName -> [PsName] -> [DataCon PsName] -> Context
contextOfData n as cs = kindCtx <> consCtx where
    kindCtx = mempty & contextTyCons . at n ?~ kind
        where kind = foldr (\_ t -> TypeT :-> t) TypeT as

    consCtx = foldMap contextOfCon cs

    contextOfCon (DataCon c as) =
        mempty & contextVars . at c ?~ ty
      where ty = foralls $ foldr (:->) base as 

    base = foldl (\f x -> AppT f (VarT x)) (VarT n) as

    foralls t = foldr ForallT t as

typeCheckRlpProgR :: (Monad m)
                  => Program PsName (RlpExpr PsName)
                  -> RLPCT m (Program PsName
                      (TypedRlpExpr PsName))
typeCheckRlpProgR p = liftHM g (inferProg p)
  where
    g = buildInitialContext p

inferProg :: Program PsName (RlpExpr PsName)
          -> HM (Program PsName (TypedRlpExpr PsName))
inferProg p = do
    g0 <- ask
    -- we only wipe the memo here as a temporary solution to the memo shadowing
    -- problem
    -- p' <- (thenWipeMemo . annotate) `traverse` etaExpandAll p
    (p',csroot) <- annotateProg (etaExpandAll p)
    traceM $ "p' : " <> show p'
    let (cs,as) = foldMap finalJudgement p' ^. lensProduct constraints assumptions
    cs' <- (\a -> cs <> csroot <> a) <$> elimAssumptionsG g0 as
    traceM $ "cs' : " <> show cs'
    sub <- solve cs'
    pure $ p' & programDecls . traversed . _FunD . _3
                %~ ((_extract %~ generaliseG g0) . fmap (sub . view _1))
  where
    etaExpandAll = programDecls . each %~ etaExpand
    thenWipeMemo a = (hmMemo .= mempty) *> a

annotateProg :: Program PsName (RlpExpr PsName)
             -> HM ( Program PsName
                      (Cofree (RlpExprF PsName) (Type PsName, PartialJudgement))
                   , [Constraint] )
annotateProg p = do
    let bs = funsToSimpleBinds (p ^. programDecls)
        (ks,xs) = unzip bs
    xs' <- annotate `traverse` xs
    let jxs = foldOf (each . _extract . _2) xs'
        txs = xs' ^.. each . _extract . _1
    cs <- elimWithBinds (ks `zip` txs) (jxs ^. assumptions)
    -- let p' = annotateDecls (ks `zip` xs') p
    -- we only wipe the memo here as a temporary solution to the memo shadowing
    -- problem
    p' <- (thenWipeMemo . annotate) `traverse` p
    p'' <- forOf (traversed . traversed . _2) p' \ j -> do
        c <- elimWithBinds (ks `zip` txs) (j ^. assumptions)
        pure $ j & constraints <>~ c
                 & assumptions %~ deleteKeys ks
    -- TODO: any remaining assumptions should be errors at this point
    pure (p'',cs)
  where
    thenWipeMemo a = (hmMemo .= mempty) *> a

-- this sucks! FunDs should probably be stored as a hashmap in Program...
annotateDecls :: [( PsName
                  , Cofree (RlpExprF PsName) (Type PsName, PartialJudgement) )]
              -> Program PsName a
              -> Program PsName
                  (Cofree (RlpExprF PsName) (Type PsName, PartialJudgement))
annotateDecls bs = programDecls . traversed . _FunD %~ \case
    (n,_,_)
        | Just e <- lookup n bs
        -> (n,[],e)

gatherBinds' :: [(PsName, RlpExpr PsName)]
             -> HM [(Type PsName, Type PsName, PartialJudgement)]
gatherBinds' = gatherBinds . fmap (uncurry simpleBind)

elimWithBinds :: [(PsName, Type PsName)]
              -> Assumptions
              -> HM [Constraint]
elimWithBinds bs jxsa = fmap concat . for bs $ \ (k,t) ->
    elimAssumptions' jxsa k t

simpleBind :: b -> a -> Binding b a
simpleBind k v = VarB (VarP k) v

funsToSimpleBinds :: [Decl PsName (RlpExpr PsName)]
                  -> [(PsName, RlpExpr PsName)]
funsToSimpleBinds = mapMaybe \case
    d@(FunD n _ _) -> Just (n, etaExpand' d)
    _              -> Nothing

simpleBindsToFuns :: [(PsName, TypedRlpExpr PsName)]
                  -> [Decl PsName (TypedRlpExpr PsName)]
simpleBindsToFuns = fmap \ (n,e) -> FunD n [] e

wrapLetrec :: [(PsName, RlpExpr PsName)] -> RlpExpr PsName
wrapLetrec ds = ds & each . _1 %~ VarP
                   & each %~ review _VarB
                   & \bs -> Finr $ LetEF Rec bs (Finl . LitF . IntL $ 123)

unwrapLetrec :: TypedRlpExpr PsName -> [(PsName, TypedRlpExpr PsName)]
unwrapLetrec (_ :< InR (LetEF _ bs _))
    = bs ^.. each . _VarB
    & each . _1 %~ view (singular _VarP)

etaExpand' :: Decl b (RlpExpr b) -> RlpExpr b
etaExpand' (FunD _ [] e) = e
etaExpand' (FunD _ as e) = Finl . LamF as' $ e
    where as' = as ^.. each . singular _VarP

etaExpand :: Decl b (RlpExpr b) -> Decl b (RlpExpr b)
etaExpand (FunD n [] e) = FunD n [] e
etaExpand (FunD n as e)
    | Right as' <- allVarP as
    = FunD n [] (Finl . LamF as' $ e)
  where
    allVarP = traverse (matching _VarP)
etaExpand a = a

liftHM :: (Monad m) => Context -> HM a -> RLPCT m a
liftHM g = liftEither . runHM g

freeVariables :: Type PsName -> HashSet PsName
freeVariables = cata \case
    VarTF x -> S.singleton x
    ForallTF x m -> S.delete x m
    vs -> fold vs

boundVariables :: Type PsName -> HashSet PsName
boundVariables = cata \case
    ForallTF x m -> S.singleton x <> m
    vs -> fold vs

freeVariablesLTR :: Type PsName -> [PsName]
freeVariablesLTR = nub . cata \case
    VarTF x -> [x]
    ForallTF x m -> m \\ [x]
    vs -> concat vs

renamePrettily' :: Type PsName -> Type PsName
renamePrettily' = join renamePrettily

-- | for some type, compute a substitution which will rename all free variables
-- for aesthetic purposes

renamePrettily :: Type PsName -> Type PsName -> Type PsName
renamePrettily root = (`evalState` alphabetNames) . (renameFree <=< renameBound)
  where
    renameBound :: Type PsName -> State [PsName] (Type PsName)
    renameBound = cata \case
        ForallTF x m -> do
            n <- getName
            ForallT n <$> (subst x (VarT n) <$> m)
        t -> embed <$> sequenceA t

    renameFree :: Type PsName -> State [PsName] (Type PsName)
    renameFree t = do
        subs <- forM (freeVariablesLTR root) $ \v -> do
            n <- getName
            pure $ Endo (subst v (VarT n))
        pure . appEndo (fold subs) $ t

    getName :: State [PsName] PsName
    getName = state (fromJust . uncons)

alphabetNames :: [PsName]
alphabetNames = alphabet ++ concatMap appendAlphabet alphabetNames
    where alphabet = [ T.pack [c] | c <- ['a'..'z'] ]
          appendAlphabet c = [ c <> c' | c' <- alphabet ]

