{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints, UndecidableInstances #-}
module Data.Pretty
    ( Out(..), Out1(..)
    , outPrec1
    , rout
    , ttext
    , Showing(..)
    -- * Out-printing lens combinators
    , hsepOf, vsepOf, vcatOf, vlinesOf
    , module Prettyprinter
    , maybeParens
    , appPrec
    , appPrec1
    )
    where
----------------------------------------------------------------------------------
import Prettyprinter
import Text.Printf
import Data.String                  (IsString(..))
import Data.Text.Lens               hiding ((:<))
import Data.Monoid                  hiding (Sum)
import Data.Bool
import Control.Lens

-- instances
import Control.Comonad.Cofree
import Data.Text                    qualified as T
import Data.Functor.Sum
import Data.Fix                     (Fix(..))
----------------------------------------------------------------------------------

class Out a where
    out :: a -> Doc ann
    outPrec :: Int -> a -> Doc ann

    {-# MINIMAL out | outPrec #-}
    out = outPrec 0
    outPrec = const out

rout :: (IsString s, Out a) => a -> s
rout = fromString . show . out

-- instance Out (Doc ann) where
--     out = id

instance Out String where
    out = pretty

instance Out T.Text where
    out = pretty

newtype Showing a = Showing a

instance (Show a) => Out (Showing a) where
    outPrec p (Showing a) = fromString $ showsPrec p a ""

deriving via Showing Int instance Out Int

class (forall a. Out a => Out (f a)) => Out1 f where
    liftOutPrec :: (Int -> a -> Doc ann) -> Int -> f a -> Doc ann

outPrec1 :: (Out1 f, Out a) => Int -> f a -> Doc ann
outPrec1 = liftOutPrec outPrec

instance (Out1 f, Out1 g, Out a) => Out (Sum f g a) where
    outPrec p (InL fa) = outPrec1 p fa
    outPrec p (InR ga) = outPrec1 p ga

instance (Out1 f, Out1 g) => Out1 (Sum f g) where
    liftOutPrec pr p (InL fa) = liftOutPrec pr p fa
    liftOutPrec pr p (InR ga) = liftOutPrec pr p ga

instance (Out (f (Fix f))) => Out (Fix f) where
    outPrec d (Fix f) = outPrec d f

--------------------------------------------------------------------------------

ttext :: Out t => t -> Doc ann
ttext = out

hsepOf :: Getting (Endo (Doc ann)) s (Doc ann) -> s -> Doc ann
hsepOf l = foldrOf l (<+>) mempty

vsepOf :: _ -> s -> Doc ann
vsepOf l = vsep . toListOf l

vcatOf :: _ -> s -> Doc ann
vcatOf l = vcat . toListOf l

vlinesOf :: Getting (Endo (Doc ann)) s (Doc ann) -> s -> Doc ann
vlinesOf l = foldrOf l (\a b -> a <> line <> b) mempty
-- hack(?) to separate chunks with a blankline

--------------------------------------------------------------------------------

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens = bool id parens

appPrec, appPrec1 :: Int
appPrec  = 10
appPrec1 = 11

instance PrintfArg (Doc ann) where
    formatArg d fmt
        | fmtChar (vFmt 'D' fmt) == 'D' = formatString (show d) fmt'
        | otherwise                     = errorBadFormat $ fmtChar fmt
      where
        fmt' = fmt { fmtChar = 's', fmtPrecision = Nothing }

