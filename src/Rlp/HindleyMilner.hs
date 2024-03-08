{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Rlp.HindleyMilner
    ( infer
    , check
    , TypeError(..)
    , HMError
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
import Text.Printf
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
import Data.Fix
import Control.Comonad.Cofree

import Compiler.RlpcError
import Rlp.AltSyntax            as Rlp
import Core.Syntax              qualified as Core
import Core.Syntax              (ExprF(..), Lit(..))
import Rlp.HindleyMilner.Types
--------------------------------------------------------------------------------

-- | Type error enum.
data TypeError
    -- | Two types could not be unified
    = TyErrCouldNotUnify (Type Name) (Type Name)
    -- | @x@ could not be unified with @t@ because @x@ occurs in @t@
    | TyErrRecursiveType Name (Type Name)
    -- | Untyped, potentially undefined variable
    | TyErrUntypedVariable Name
    | TyErrMissingTypeSig Name
    deriving (Show)

instance IsRlpcError TypeError where
    liftRlpcError = \case
        -- todo: use anti-parser instead of show
        TyErrCouldNotUnify t u -> Text
            [ T.pack $ printf "Could not match type `%s` with `%s`."
                              (rpretty @String t) (rpretty @String u)
            , "Expected: " <> rpretty t
            , "Got: " <> rpretty u
            ]
        TyErrUntypedVariable n -> Text
            [ "Untyped (likely undefined) variable `" <> n <> "`"
            ]
        TyErrRecursiveType t x -> Text
            [ T.pack $ printf "Recursive type: `%s' occurs in `%s'"
                              (rpretty @String t) (rpretty @String x)
            ]

-- | Synonym for @Errorful [TypeError]@. This means an @HMError@ action may
-- throw any number of fatal or nonfatal errors. Run with @runErrorful@.
type HMError = Errorful TypeError

check = undefined

fixCofree :: (Functor f, Functor g)
          => Iso (Fix f) (Fix g) (Cofree f ()) (Cofree g b)
fixCofree = iso sa bt where
    sa = foldFix (() :<)
    bt (_ :< as) = Fix $ bt <$> as

type Gather t = WriterT PartialJudgement (HM t)

addConstraint :: Constraint -> Gather t ()
addConstraint = tell . ($ mempty) . (_PartialJudgement .~) . pure

lookupContext :: Applicative m => PsName -> Context' -> m (Type PsName)
lookupContext n g = maybe (error "undefined variable") pure $
    H.lookup n g

-- | 'gather', but memoise the result. All recursive calls should be to
-- 'gather'', not 'gather'!

gather' :: Context'
        -> Fix (RlpExprF PsName)
        -> Gather (Fix (RlpExprF PsName)) (Type PsName)
gather' g e = do
    t <- listen $ gather g e
    lift . tell $ H.singleton e t
    pure (t ^. _1)

gather :: Context'
       -> Fix (RlpExprF PsName)
       -> Gather (Fix (RlpExprF PsName)) (Type PsName)

gather g (Finl (LitF (IntL _))) = pure IntT

gather g (Finl (VarF n)) = lookupContext n g

gather g (Finl (AppF f x)) = do
    ty <- lift freshTv
    tf <- gather' g f
    tx <- gather' g x
    addConstraint $ Equality tf (tx :-> ty)
    pure ty

gather g (Finl (LamF xs e)) = do
    bs <- for xs \n -> do
        tx <- lift freshTv
        pure (n, tx)
    let g' = H.fromList bs <> g
        txs = bs ^.. each . _2
    te <- gather' g' e
    pure $ foldr (:->) te txs

gather g (Finr (CaseEF e as)) = do
    undefined

gatherA :: Context'
        -> Alter PsName (Fix (RlpExprF PsName))
        -> Gather (Fix (RlpExprF PsName)) (Type PsName)
gatherA = undefined

type Subst = Context'

applySubst :: Subst -> Type PsName -> Type PsName
applySubst = flip $ ifoldr subst

composeSubst :: Subst -> Subst -> Subst
composeSubst = H.union

subst :: (Eq b) => b -> Type b -> Type b -> Type b
subst n t' = para \case
    VarTF x               | n == x -> t'
    -- here `pre` is the oringal, unsubstituted type
    ForallTF x (pre,post) | n == x -> ForallT x pre
    t                              -> embed $ fmap snd t

mgu :: Type PsName -> Type PsName -> Maybe Subst

mgu (VarT n) t = Just $ H.singleton n t
mgu t (VarT n) = Just $ H.singleton n t

mgu (ConT a) (ConT b) | a == b = Just mempty

mgu (a :-> b) (a' :-> b') = do
    sa <- a `mgu` a'
    sb <- applySubst sa b `mgu` applySubst sa b'
    pure $ sa `composeSubst` sb

mgu _ _ = Nothing

solve :: [Constraint] -> Maybe Subst
solve = foldM go mempty where
    go s (Equality a b) = applySubst s a `mgu` applySubst s b

infer :: RlpExpr PsName -> Cofree (RlpExprF PsName) (Type PsName)
infer = undefined

demoContext :: Context'
demoContext = H.fromList
    [ ("id", ForallT "a" $ VarT "a" :-> VarT "a")
    ]

{--

type TC t = State (TypeState t (Type PsName, PartialJudgement))
                  (Type PsName, PartialJudgement)

freshTv :: State (TypeState t m) (Type PsName)
freshTv = do
    n <- use tsUnique
    tsUnique %= succ
    pure . VarT $ "$a" <> T.pack (show n)

memoisedTC :: (Hashable a) => (a -> TC a) -> a -> TC a
memoisedTC k a = do
    m <- use tsMemo
    r <- k a
    tsMemo . at a %= \case
        Just c -> Just c
        Nothing -> Just r
    pure r

gather :: Fix (RlpExprF PsName) -> TC (Fix (RlpExprF PsName)) 

gather (Fix (InL (Core.LitF (Core.IntL _)))) =
    pure (ConT "Int#", mempty)

gather (Fix (InL (Core.VarF n))) = do
    tv <- freshTv
    let j = mempty & assumptions .~ H.singleton n tv
    pure (tv, j)

gather (Fix (InL (Core.AppF f x))) = do
    tv <- freshTv
    (tf,j) <- memoisedTC gather f
    (tx,j') <- memoisedTC gather x
    let j'' = mempty & constraints .~ S.singleton (Equality tf $ tx :-> tv)
    pure (tv, j <> j' <> j'')

--}

