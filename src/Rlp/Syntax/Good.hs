{-# LANGUAGE TemplateHaskell #-}
module Rlp.Syntax.Good
    ( Decl(..), Program(..)
    , programDecls
    , Mistake(..)
    )
    where
--------------------------------------------------------------------------------
import Data.Kind
import Control.Lens
import Rlp.Syntax.Types (NameP)
import Rlp.Syntax.Types qualified as Rlp
--------------------------------------------------------------------------------

data Program b a = Program
    { _programDecls :: [Decl b a]
    }

data Decl p a = FunD   (NameP p) [Rlp.Pat p] a
              | TySigD [NameP p] (Rlp.Ty p)
              | DataD  (NameP p) [NameP p] [Rlp.ConAlt p]
              | InfixD Rlp.Assoc Int (NameP p)

type Where p a = [Binding p a]

data Binding p a = PatB (Rlp.Pat p) a
    deriving (Functor, Foldable, Traversable)

makeLenses ''Program

class Mistake a where
    type family Ammend a :: Type
    ammendMistake :: a -> Ammend a

instance Mistake (Rlp.Program p a) where
    type Ammend (Rlp.Program p a) = Program p (Rlp.Expr' p a)

    ammendMistake p = Program
        { _programDecls = ammendMistake <$> Rlp._programDecls p
        }

instance Mistake (Rlp.Decl p a) where
    type Ammend (Rlp.Decl p a) = Decl p (Rlp.Expr' p a)

    ammendMistake = \case
        Rlp.FunD n as e _  -> FunD n as e
        Rlp.TySigD ns t    -> TySigD ns t
        Rlp.DataD n as cs  -> DataD n as cs
        Rlp.InfixD ass p n -> InfixD ass p n

instance Mistake (Rlp.Binding p a) where
    type Ammend (Rlp.Binding p a) = Binding p (Rlp.ExprF p a)

    ammendMistake = \case
        Rlp.PatB k v -> PatB k v

