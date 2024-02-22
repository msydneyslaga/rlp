{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Rlp.Syntax.Backstage
    ( strip
    )
    where
--------------------------------------------------------------------------------
import Data.Fix                     hiding (cata)
import Data.Functor.Classes
import Data.Functor.Foldable
import Rlp.Syntax.Types
import Text.Show.Deriving
import Language.Haskell.TH.Syntax   (Lift)
--------------------------------------------------------------------------------

-- oprhan instances because TH

instance (Show (NameP p)) => Show1 (Alt p) where
    liftShowsPrec = $(makeLiftShowsPrec ''Alt)

instance (Show (NameP p)) => Show1 (Binding p) where
    liftShowsPrec = $(makeLiftShowsPrec ''Binding)

instance (Show (NameP p)) => Show1 (ExprF p) where
    liftShowsPrec = $(makeLiftShowsPrec ''ExprF)

deriving instance (Lift (NameP p), Lift a) => Lift (Expr' p a)
deriving instance (Lift (NameP p), Lift a) => Lift (Decl p a)
deriving instance (Show (NameP p), Show a) => Show (Decl p a)

deriving instance (Show (NameP p), Show a) => Show (Program p a)

strip :: Functor f => Cofree f a -> Fix f
strip (_ :< as) = Fix $ strip <$> as

