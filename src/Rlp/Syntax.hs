-- recursion-schemes
{-# LANGUAGE DeriveTraversable, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
module Rlp.Syntax
    (
      NameP
    , Assoc(..)
    , ConAlt(..)
    , Alt(..)
    , Ty(..)
    , Binding(..)
    , Expr(..), Expr', ExprF(..)
    , Lit(..)
    , Pat(..)
    , Decl(..)
    , Program(..)
    , Where

    -- * Re-exports
    , Cofree(..)
    , Trans.Cofree.CofreeF
    , pattern (:<$)
    , SrcSpan(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Kind                    (Type)
import GHC.Generics
import Language.Haskell.TH.Syntax   (Lift)
import Control.Lens

import Control.Comonad.Trans.Cofree qualified as Trans.Cofree
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)

import Compiler.Types               (SrcSpan(..), Located(..))
import Core.Syntax                  qualified as Core
import Core                         (Rec(..), HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

type family NameP p

data Program p = Program
    { _programDecls :: [Decl p]
    }

deriving instance (Show (NameP p)) => Show (Program p)

data Decl p = FunD   (NameP p) [Pat p] (Expr p) (Maybe (Where p))
            | TySigD [NameP p] (Ty p)
            | DataD  (NameP p) [NameP p] [ConAlt p]
            | InfixD Assoc Int (NameP p)

deriving instance (Lift (NameP p)) => Lift (Decl p)
deriving instance (Show (NameP p)) => Show (Decl p)

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

data Ty p = TyCon (NameP p)

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

pattern (:<$) :: a -> f b -> Trans.Cofree.CofreeF f a b
pattern a :<$ b = a Trans.Cofree.:< b

--------------------------------------------------------------------------------

makeBaseFunctor ''Expr
makeLenses ''Program

type Expr' p = Cofree (ExprF p)

