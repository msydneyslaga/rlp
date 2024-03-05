{-# LANGUAGE TemplateHaskell #-}
module Misc.Lift1
    ( Lift1(..), lift1
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

-- instances
import Data.Fix
import Data.Functor.Sum
--------------------------------------------------------------------------------

class Lift1 (f :: Data.Kind.Type -> Data.Kind.Type) where
    -- lift1 :: (Quote m, Lift t) => f t -> m Exp
    liftLift :: (Quote m) => (a -> m Exp) -> f a -> m Exp

lift1 :: (Lift1 f, Lift a, Quote m) => f a -> m Exp
lift1 = liftLift lift

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

instance Lift1 [] where
    liftLift lf []     = pure $ ConE '[]
    liftLift lf (a:as) = liftCon2 '(:) (lf a) (liftLift lf as)

instance (Lift1 f, Lift1 g) => Lift1 (Sum f g) where
    liftLift lf (InL fa) = liftCon 'InL $ liftLift lf fa
    liftLift lf (InR ga) = liftCon 'InR $ liftLift lf ga

