module Data.Pretty
    ( Pretty(..)
    , ttext
    -- * Pretty-printing lens combinators
    , hsepOf, vsepOf
    , module Text.PrettyPrint
    , maybeParens
    )
    where
----------------------------------------------------------------------------------
import Text.PrettyPrint             hiding ((<>))
import Text.PrettyPrint.HughesPJ    hiding ((<>))
import Data.String                  (IsString(..))
import Data.Text.Lens
import Data.Monoid
import Data.Text                    qualified as T
import Control.Lens
----------------------------------------------------------------------------------

class Pretty a where
    pretty :: a -> Doc
    prettyPrec :: Int -> a -> Doc

    {-# MINIMAL pretty | prettyPrec #-}
    pretty = prettyPrec 0
    prettyPrec a _ = pretty a

instance Pretty String where
    pretty = Text.PrettyPrint.text

instance Pretty T.Text where
    pretty = Text.PrettyPrint.text . view unpacked

newtype Showing a = Showing a

instance (Show a) => Pretty (Showing a) where
    prettyPrec p (Showing a) = fromString $ showsPrec p a ""

deriving via Showing Int instance Pretty Int

--------------------------------------------------------------------------------

ttext :: Pretty t => t -> Doc
ttext = pretty

hsepOf :: Getting (Endo Doc) s Doc -> s -> Doc
hsepOf l = foldrOf l (<+>) mempty

vsepOf :: Getting (Endo Doc) s Doc -> s -> Doc
vsepOf l = foldrOf l ($+$) mempty

