{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
module Core.Syntax
    ( Expr(..)
    , Literal(..)
    , pattern (:$)
    , Binding(..)
    , AltCon(..)
    , pattern (:=)
    , Rec(..)
    , Alter(..)
    , Name
    , Tag
    , ScDef(..)
    , Module(..)
    , Program(..)
    , Program'
    , programScDefs
    , Expr'
    , ScDef'
    , Alter'
    , Binding'
    , HasRHS(_rhs)
    , HasLHS(_lhs)
    )
    where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import GHC.Generics
import Data.List                    (intersperse)
import Data.Function                ((&))
import Data.String
-- Lift instances for the Core quasiquoters
import Lens.Micro
import Language.Haskell.TH.Syntax   (Lift)
----------------------------------------------------------------------------------

data Expr b = Var Name
            | Con Tag Int -- Con Tag Arity
            | Case (Expr b) [Alter b]
            | Lam [b] (Expr b)
            | Let Rec [Binding b] (Expr b)
            | App (Expr b) (Expr b)
            | LitE Literal
            deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Expr b)

infixl 2 :$
pattern (:$) :: Expr b  -> Expr b  -> Expr b 
pattern f :$ x = App f x

{-# COMPLETE Binding :: Binding #-}
{-# COMPLETE (:=) :: Binding #-}
data Binding b = Binding b (Expr b)
    deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Binding b)

infixl 1 :=
pattern (:=) :: b -> (Expr b) -> (Binding b)
pattern k := v = Binding k v

data Alter b = Alter AltCon [b] (Expr b)
    deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Alter b)

data Rec = Rec
         | NonRec
         deriving (Show, Read, Eq, Lift)

data AltCon = AltData Tag
            | AltLiteral Literal
            | Default
            deriving (Show, Read, Eq, Lift)

data Literal = IntL Int
    deriving (Show, Read, Eq, Lift)

type Name = String
type Tag = Int

data ScDef b = ScDef b [b] (Expr b)
    deriving (Show, Lift)

data Module b = Module (Maybe (Name, [Name])) (Program b)
    deriving (Show, Lift)

newtype Program b = Program [ScDef b]
    deriving (Show, Lift)

programScDefs :: Lens' (Program b) [ScDef b]
programScDefs = lens coerce (const coerce)

type Program' = Program Name
type Expr' = Expr Name
type ScDef' = ScDef Name
type Alter' = Alter Name
type Binding' = Binding Name

instance IsString (Expr b) where
    fromString = Var

instance Semigroup (Program b) where
    (<>) = coerce $ (<>) @[ScDef b] 

instance Monoid (Program b) where
    mempty = Program []

----------------------------------------------------------------------------------

class HasRHS s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _rhs :: Lens s t a b

instance HasRHS (Alter b) (Alter b) (Expr b) (Expr b) where
    _rhs = lens
        (\ (Alter _ _ e) -> e)
        (\ (Alter t as _) e' -> Alter t as e')

instance HasRHS (ScDef b) (ScDef b) (Expr b) (Expr b) where
    _rhs = lens
        (\ (ScDef _ _ e) -> e)
        (\ (ScDef n as _) e' -> ScDef n as e')

instance HasRHS (Binding b) (Binding b) (Expr b) (Expr b) where
    _rhs = lens
        (\ (_ := e) -> e)
        (\ (k := _) e' -> k := e')

class HasLHS s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _lhs :: Lens s t a b

instance HasLHS (Alter b) (Alter b) (AltCon, [b]) (AltCon, [b]) where
    _lhs = lens
        (\ (Alter a bs _) -> (a,bs))
        (\ (Alter _ _ e) (a',bs') -> Alter a' bs' e)

instance HasLHS (ScDef b) (ScDef b) (b, [b]) (b, [b]) where
    _lhs = lens
        (\ (ScDef n as _) -> (n,as))
        (\ (ScDef _ _ e) (n',as') -> (ScDef n' as' e))

