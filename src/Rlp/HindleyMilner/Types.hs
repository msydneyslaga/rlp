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
import Control.Monad.State

import Control.Lens             hiding (Context', Context)

import Rlp.AltSyntax
--------------------------------------------------------------------------------

type Context' = HashMap PsName (Type PsName)

data Constraint = Equality (Type PsName) (Type PsName)
    deriving (Eq, Generic, Show)

newtype PartialJudgement = PartialJudgement Constraints
    deriving (Generic, Show)
    deriving (Semigroup, Monoid)
        via Generically PartialJudgement

instance Hashable Constraint

type Constraints = HashSet Constraint

type Memo t = HashMap t (Type PsName, PartialJudgement)

newtype HM t a = HM { unHM :: Int -> Memo t -> (a, Int, Memo t) }

runHM :: (Hashable t) => HM t a -> (a, Memo t)
runHM hm = let (a,_,m) = unHM hm 0 mempty in (a,m)

instance Functor (HM t) where
    fmap f (HM h) = HM \n m -> h n m & _1 %~ f

instance Applicative (HM t) where
    pure a = HM \n m -> (a,n,m)
    HM hf <*> HM ha = HM \n m ->
        let (f',n',m') = hf n m
            (a,n'',m'') = ha n' m'
        in (f' a, n'', m'')

instance Monad (HM t) where
    HM ha >>= k = HM \n m ->
        let (a,n',m') = ha n m
            (a',n'',m'') = unHM (k a) n' m'
        in (a',n'', m'')

instance Hashable t => MonadWriter (Memo t) (HM t) where
    -- IMPORTAN! (<>) is left-biased for HashMap! append `w` to the RIGHt!
    writer (a,w) = HM \n m -> (a,n,m <> w)
    listen ma = HM \n m ->
        let (a,n',m') = unHM ma n m
        in ((a,m'),n',m')
    pass maww = HM \n m ->
        let ((a,ww),n',m') = unHM maww n m
        in (a,n',ww m')

instance MonadState Int (HM t) where
    state f = HM \n m ->
        let (a,n') = f n
        in (a,n',m)

freshTv :: HM t (Type PsName)
freshTv = do
    n <- get
    modify succ
    pure . VarT $ "$a" <> T.pack (show n)

makePrisms ''PartialJudgement

