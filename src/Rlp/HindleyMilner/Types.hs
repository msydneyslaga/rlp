{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Rlp.HindleyMilner.Types
    where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.HashMap.Strict      (HashMap)
import Data.HashMap.Strict      qualified as H
import Data.HashSet             (HashSet)
import Data.HashSet             qualified as S
import GHC.Generics             (Generic(..), Generically(..))
import Data.Kind                qualified
import Data.Text                qualified as T
import Control.Monad.Writer
import Control.Monad.Accum
import Control.Monad.Trans.Accum
import Control.Monad.Errorful
import Control.Monad.State
import Control.Monad.Reader
import Text.Printf
import Data.Pretty
import Data.Function

import Control.Lens             hiding (Context', Context)

import Compiler.RlpcError
import Rlp.AltSyntax
--------------------------------------------------------------------------------

data Context = Context
    { _contextVars      :: HashMap PsName (Type PsName)
    , _contextTyVars    :: HashMap PsName (Type PsName)
    , _contextTyCons    :: HashMap PsName (Kind PsName)
    }
    deriving (Show, Generic)
    deriving (Semigroup, Monoid)
        via Generically Context

data Constraint = Equality (Type PsName) (Type PsName)
                | GeneralisedEquality (Type PsName) (Type PsName)
                deriving (Eq, Generic, Show)

type Assumptions = HashMap PsName [Type PsName]

data PartialJudgement = PartialJudgement
    { _constraints :: [Constraint]
    , _assumptions :: Assumptions
    }
    deriving (Generic, Show)
    deriving (Monoid)
        via Generically PartialJudgement

instance Semigroup PartialJudgement where
    a <> b = PartialJudgement
        { _constraints = ((<>) `on` _constraints) a b
        , _assumptions = (H.unionWith (<>) `on` _assumptions) a b
        }

instance Hashable Constraint

type Memo = HashMap (RlpExpr PsName) (Type PsName, PartialJudgement)

data HMState = HMState
    { _hmMemo :: Memo
    , _hmUniq :: Int
    }
    deriving Show

newtype HM a = HM {
        unHM :: ErrorfulT TypeError
            (ReaderT Context (State HMState)) a
    }
    deriving (Functor, Applicative, Monad)
    deriving ( MonadReader Context
             , MonadState HMState
             , MonadErrorful TypeError
             )

-- | Type error enum.
data TypeError
    -- | Two types could not be unified
    = TyErrCouldNotUnify (Type Name) (Type Name)
    -- | @x@ could not be unified with @t@ because @x@ occurs in @t@
    | TyErrRecursiveType Name (Type Name)
    -- | Untyped, potentially undefined variable
    | TyErrUntypedVariable Name
    | TyErrMissingTypeSig Name
    | TyErrNonHomogenousCaseAlternatives (RlpExpr PsName)
    deriving (Show)

instance IsRlpcError TypeError where
    liftRlpcError = \case
        -- todo: use anti-parser instead of show
        TyErrCouldNotUnify t u -> Text
            [ T.pack $ printf "Could not match type `%s` with `%s`."
                              (rout @String t) (rout @String u)
            , "Expected: " <> rout t
            , "Got: " <> rout u
            ]
        TyErrUntypedVariable n -> Text
            [ "Untyped (likely undefined) variable `" <> n <> "`"
            ]
        TyErrRecursiveType t x -> Text
            [ T.pack $ printf "Recursive type: `%s' occurs in `%s'"
                              (rout @String t) (rout @String x)
            ]

runHM :: Context -> HM a -> Either [TypeError] a
runHM g e = maybe (Left es) Right ma
    where
        (ma,es) =  (`evalState` (HMState mempty 0))
                    . (`runReaderT` g) . runErrorfulT $ unHM e

runHM' :: HM a -> Either [TypeError] a
runHM' = runHM mempty

makePrisms ''PartialJudgement
makeLenses ''PartialJudgement
makeLenses ''Context
makePrisms ''Constraint
makePrisms ''TypeError
makeLenses ''HMState

supplement :: [(PsName, Type PsName)] -> Context -> Context
supplement bs = contextVars %~ (H.fromList bs <>)

demoContext :: Context
demoContext = mempty
    & contextVars .~
        [ ("+#", IntT :-> IntT :-> IntT)
        , ("Nil", ForallT "a" $ ConT "List" `AppT` VarT "a")
        ]
    & contextTyCons .~
        [ ("List", TypeT :-> TypeT)
        ]

constraintTypes :: Traversal' Constraint (Type PsName)
constraintTypes k (Equality s t) = Equality <$> k s <*> k t
constraintTypes k (GeneralisedEquality s t) =
    GeneralisedEquality <$> k s <*> k t

instance Out Constraint where
    out (Equality s t) =
        hsep [outPrec appPrec1 s, "~", outPrec appPrec1 t]

tvNameOfInt :: Int -> PsName
tvNameOfInt n = "$a" <> T.pack (show n)

freshTv :: HM (Type PsName)
freshTv = do
    n <- use hmUniq
    hmUniq %= succ
    pure (VarT $ tvNameOfInt n)

listenFreshTvs :: HM a -> HM (a, [Type PsName])
listenFreshTvs hm = listenFreshTvNames hm & mapped . _2 . each %~ VarT

listenFreshTvNames :: HM a -> HM (a, [PsName])
listenFreshTvNames hm = do
    n <- use hmUniq
    a <- hm
    n' <- use hmUniq
    pure (a, [ tvNameOfInt k | k <- [n .. pred n'] ])

