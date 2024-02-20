-- recursion-schemes
{-# LANGUAGE DeriveTraversable, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
module Rlp.Syntax.Types
    (
      NameP
    , SimpleP
    , Assoc(..)
    , ConAlt(..)
    , Alt(..)
    , Ty(..)
    , Binding(..)
    , Expr(..), Expr', ExprF(..)
    , Rec(..)
    , Lit(..)
    , Pat(..)
    , Decl(..)
    , Program(..)
    , Where

    -- * Re-exports
    , Cofree(..)
    , Trans.Cofree.CofreeF
    , SrcSpan(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Fix
import Data.Kind                    (Type)
import GHC.Generics
import Language.Haskell.TH.Syntax   (Lift)
import Control.Lens                 hiding ((:<))

import Control.Comonad.Trans.Cofree qualified as Trans.Cofree
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)

import Compiler.Types               (SrcSpan(..), Located(..))
import Core.Syntax                  qualified as Core
import Core                         (Rec(..), HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

data SimpleP

type instance NameP SimpleP = String

type family NameP p

data Expr p = LetE   Rec [Binding p] (Expr p)
            | VarE   (NameP p)
            | LamE   [Pat p] (Expr p)
            | CaseE  (Expr p) [Alt p]
            | IfE    (Expr p) (Expr p) (Expr p)
            | AppE   (Expr p) (Expr p)
            | LitE   (Lit p)
            | ParE   (Expr p)
            | InfixE (NameP p) (Expr p) (Expr p)
            deriving (Generic)

deriving instance (Lift (NameP p)) => Lift (Expr p)
deriving instance (Show (NameP p)) => Show (Expr p)

data ConAlt p = ConAlt (NameP p) [Ty p]

deriving instance (Lift (NameP p)) => Lift (ConAlt p)
deriving instance (Show (NameP p)) => Show (ConAlt p)

data Ty p = ConT (NameP p)
          | VarT (NameP p)
          | FunT (Ty p) (Ty p)
          | AppT (Ty p) (Ty p)

deriving instance (Show (NameP p)) => Show (Ty p)
deriving instance (Lift (NameP p)) => Lift (Ty p)

data Pat p = VarP (NameP p)
           | LitP (Lit p)
           | ConP (NameP p) [Pat p]

deriving instance (Lift (NameP p)) => Lift (Pat p)
deriving instance (Show (NameP p)) => Show (Pat p)

data Binding p = PatB (Pat p) (Expr p)

deriving instance (Lift (NameP p)) => Lift (Binding p)
deriving instance (Show (NameP p)) => Show (Binding p)

data Lit p = IntL Int
    deriving Show

deriving instance (Lift (NameP p)) => Lift (Lit p)

data Alt p = AltA (Pat p) (Expr p) (Maybe (Where p))

deriving instance (Show (NameP p)) => Show (Alt p)
deriving instance (Lift (NameP p)) => Lift (Alt p)

type Where p = [Binding p]

data Assoc = InfixL | InfixR | Infix
    deriving (Lift, Show)

makeBaseFunctor ''Expr

deriving instance (Show (NameP p), Show a) => Show (ExprF p a)
deriving instance (Lift (NameP p), Lift a) => Lift (ExprF p a)

--------------------------------------------------------------------------------

data Program p a = Program
    { _programDecls :: [Decl p a]
    }

data Decl p a = FunD   (NameP p) [Pat p] (Expr' p a) (Maybe (Where p))
              | TySigD [NameP p] (Ty p)
              | DataD  (NameP p) [NameP p] [ConAlt p]
              | InfixD Assoc Int (NameP p)

type Expr' p = Cofree (ExprF p)

makeLenses ''Program

loccof :: Iso' (Cofree f SrcSpan) (Located (f (Cofree f SrcSpan)))
loccof = iso sa bt where
    sa :: Cofree f SrcSpan -> Located (f (Cofree f SrcSpan))
    sa (ss :< as) = Located ss as

    bt :: Located (f (Cofree f SrcSpan)) -> Cofree f SrcSpan
    bt (Located ss as) = ss :< as

