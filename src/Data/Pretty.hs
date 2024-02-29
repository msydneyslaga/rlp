{-# LANGUAGE QuantifiedConstraints #-}
module Data.Pretty
    ( Pretty(..)
    , rpretty
    , ttext
    -- * Pretty-printing lens combinators
    , hsepOf, vsepOf
    , vcatOf
    , vlinesOf
    , vsepTerm
    , module Text.PrettyPrint
    , maybeParens
    , appPrec
    , appPrec1
    )
    where
----------------------------------------------------------------------------------
import Text.PrettyPrint             hiding ((<>))
import Text.PrettyPrint.HughesPJ    hiding ((<>))
import Text.Printf
import Data.String                  (IsString(..))
import Data.Text.Lens               hiding ((:<))
import Data.Monoid
import Control.Lens

-- instances
import Control.Comonad.Cofree
import Data.Text                    qualified as T
----------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    {-# MINIMAL pretty | prettyPrec #-}
    pretty = prettyPrec 0
    prettyPrec = const pretty

rpretty :: (IsString s, Pretty a) => a -> s
rpretty = fromString . render . pretty

instance Pretty String where
    pretty = Text.PrettyPrint.text

instance Pretty T.Text where
    pretty = Text.PrettyPrint.text . view unpacked

newtype Showing a = Showing a

instance (Show a) => Pretty (Showing a) where
    prettyPrec p (Showing a) = fromString $ showsPrec p a ""

deriving via Showing Int instance Pretty Int

class (forall a. Pretty a => Pretty (f a)) => Pretty1 f where
    liftPrettyPrec :: (Int -> a -> Doc) -> f a -> Doc

--------------------------------------------------------------------------------

ttext :: Pretty t => t -> Doc
ttext = pretty

hsepOf :: Getting (Endo Doc) s Doc -> s -> Doc
hsepOf l = foldrOf l (<+>) mempty

vsepOf :: Getting (Endo Doc) s Doc -> s -> Doc
vsepOf l = foldrOf l ($+$) mempty

vcatOf :: Getting (Endo Doc) s Doc -> s -> Doc
vcatOf l = foldrOf l ($$) mempty

vlinesOf :: Getting (Endo Doc) s Doc -> s -> Doc
vlinesOf l = foldrOf l (\a b -> a $+$ "" $+$ b) mempty
-- hack(?) to separate chunks with a blankline

vsepTerm :: Doc -> Doc -> Doc -> Doc
vsepTerm term a b = (a <> term) $+$ b

--------------------------------------------------------------------------------

appPrec, appPrec1 :: Int
appPrec  = 10
appPrec1 = 11

instance PrintfArg Doc where
    formatArg d fmt
        | fmtChar (vFmt 'D' fmt) == 'D' = formatString (render d) fmt'
        | otherwise                     = errorBadFormat $ fmtChar fmt
      where
        fmt' = fmt { fmtChar = 's', fmtPrecision = Nothing }

