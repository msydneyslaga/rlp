-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
  , TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
module Rlp.Syntax
    (
      NameP
    , Assoc(..)
    , ConAlt(..)
    , Alt(..)
    , Ty(..)
    , Binding(..)
    , Expr(..)
    , Lit(..)
    , Pat(..)
    , Decl(..)
    , Program(..)
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Kind                    (Type)
import GHC.Generics
import Language.Haskell.TH.Syntax   (Lift)
import Control.Lens
import Core.Syntax                  qualified as Core
import Core                         (Rec(..), HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

type PsName = Text
type family NameP p

data Program p

data Decl p = FunD   (NameP p) [Pat p] (Expr p) (Maybe (Where p))
            | TySigD [NameP p] (Ty p)
            | DataD  (NameP p) [NameP p] [ConAlt p]
            | InfixD Assoc Int (NameP p)

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

deriving instance (Show (NameP p)) => Show (Expr p)

data ConAlt p = ConAlt (NameP p) [Ty p]

deriving instance (Show (NameP p)) => Show (ConAlt p)

data Ty p
    deriving Show

data Pat p = VarP (NameP p)
           | LitP (Lit p)
           | ConP (NameP p) [Pat p]

deriving instance (Show (NameP p)) => Show (Pat p)

data Binding p = PatB (Pat p) (Expr p)

deriving instance (Show (NameP p)) => Show (Binding p)

data Lit p = IntL Int
    deriving Show

data Alt p = AltA (Pat p) (Expr p) (Maybe (Where p))

deriving instance (Show (NameP p)) => Show (Alt p)

type Where p = [Binding p]

data Assoc = InfixL | InfixR | Infix
    deriving (Lift, Show)

