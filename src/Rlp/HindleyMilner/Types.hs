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
import Control.Monad.Errorful
import Control.Monad.State
import Text.Printf
import Data.Pretty

import Control.Lens             hiding (Context', Context)

import Compiler.RlpcError
import Rlp.AltSyntax
--------------------------------------------------------------------------------

data Context = Context
    { _contextVars      :: HashMap PsName (Type PsName)
    , _contextTyVars    :: HashMap PsName (Type PsName)
    }

data Constraint = Equality (Type PsName) (Type PsName)
    deriving (Eq, Generic, Show)

data PartialJudgement = PartialJudgement [Constraint]
                                         (HashMap PsName [Type PsName])
    deriving (Generic, Show)
    deriving (Semigroup, Monoid)
        via Generically PartialJudgement

instance Hashable Constraint

type HM = ErrorfulT TypeError (State Int)

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

-- type Memo t = HashMap t (Type PsName, PartialJudgement)

-- newtype HM t a = HM { unHM :: Int -> Memo t -> (a, Int, Memo t) }

-- runHM :: (Hashable t) => HM t a -> (a, Memo t)
-- runHM hm = let (a,_,m) = unHM hm 0 mempty in (a,m)

-- instance Functor (HM t) where
--     fmap f (HM h) = HM \n m -> h n m & _1 %~ f

-- instance Applicative (HM t) where
--     pure a = HM \n m -> (a,n,m)
--     HM hf <*> HM ha = HM \n m ->
--         let (f',n',m') = hf n m
--             (a,n'',m'') = ha n' m'
--         in (f' a, n'', m'')

-- instance Monad (HM t) where
--     HM ha >>= k = HM \n m ->
--         let (a,n',m') = ha n m
--             (a',n'',m'') = unHM (k a) n' m'
--         in (a',n'', m'')

-- instance Hashable t => MonadWriter (Memo t) (HM t) where
--     -- IMPORTAN! (<>) is left-biased for HashMap! append `w` to the RIGHt!
--     writer (a,w) = HM \n m -> (a,n,m <> w)
--     listen ma = HM \n m ->
--         let (a,n',m') = unHM ma n m
--         in ((a,m'),n',m')
--     pass maww = HM \n m ->
--         let ((a,ww),n',m') = unHM maww n m
--         in (a,n',ww m')

-- instance MonadState Int (HM t) where
--     state f = HM \n m ->
--         let (a,n') = f n
--         in (a,n',m)

freshTv :: HM (Type PsName)
freshTv = do
    n <- get
    modify succ
    pure . VarT $ "$a" <> T.pack (show n)

runHM' :: HM a -> Either [TypeError] a
runHM' e = maybe (Left es) Right ma
    where
        (ma,es) = (`evalState` 0) . runErrorfulT $ e

-- makePrisms ''PartialJudgement

makeLenses ''Context

