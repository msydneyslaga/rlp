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
import Control.Lens             hiding (Context', Context, (:<))
import Control.Monad.Errorful
import Control.Monad.State
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
import GHC.Generics             (Generic(..), Generically(..))

import Data.Functor
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

infer = undefined
check = undefined

fixCofree :: (Functor f, Functor g)
          => Iso (Fix f) (Fix g) (Cofree f ()) (Cofree g b)
fixCofree = iso sa bt where
    sa = foldFix (() :<)
    bt (_ :< as) = Fix $ bt <$> as

type Gather t = WriterT PartialJudgement (HM t)

addConstraint :: Constraint -> Gather t ()
addConstraint = tell . ($ mempty) . (_PartialJudgement .~) . S.singleton

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
    tv <- lift freshTv
    tf <- gather' g f
    tx <- gather' g x
    addConstraint $ Equality tf (tx :-> tv)
    pure tv

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

