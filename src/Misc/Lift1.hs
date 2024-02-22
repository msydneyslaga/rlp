{-# LANGUAGE TemplateHaskell #-}
module Misc.Lift1
    ( Lift1(..)
    , liftCon, liftCon2, liftCon3
    , Lift(..)
    )
    where
--------------------------------------------------------------------------------
import Language.Haskell.TH              hiding (Type, Name)
import Language.Haskell.TH.Syntax       hiding (Type, Name)
import Language.Haskell.TH.Syntax       qualified as TH
import Language.Haskell.TH.Quote
import Data.Kind                        qualified
import GHC.Generics

import Data.Fix
--------------------------------------------------------------------------------

class Lift1 (f :: Data.Kind.Type -> Data.Kind.Type) where
    lift1 :: (Quote m, Lift t) => f t -> m Exp

liftCon :: Quote m => TH.Name -> m Exp -> m Exp
liftCon n = fmap (AppE (ConE n))

liftCon2 :: Quote m => TH.Name -> m Exp -> m Exp -> m Exp
liftCon2 n a b = do
    a' <- a
    b' <- b
    pure $ ConE n `AppE` a' `AppE` b'

liftCon3 :: Quote m => TH.Name -> m Exp -> m Exp -> m Exp -> m Exp
liftCon3 n a b c = do
    a' <- a
    b' <- b
    c' <- c
    pure $ ConE n `AppE` a' `AppE` b' `AppE` c'

instance Lift1 f => Lift (Fix f) where
    lift (Fix f) = AppE (ConE 'Fix) <$> lift1 f


