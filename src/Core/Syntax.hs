{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
module Core.Syntax
    ( Expr(..)
    , Type(..)
    , pattern TyInt
    , Lit(..)
    , pattern (:$)
    , pattern (:@)
    , pattern (:->)
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
    , unliftScDef
    , programScDefs
    , programTypeSigs
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
import Data.List                    (intersperse)
import Data.Function                ((&))
import Data.String
import Data.HashMap.Strict          qualified as H
import Data.Hashable
import Data.Text                    qualified as T
import Data.Char
-- Lift instances for the Core quasiquoters
import Language.Haskell.TH.Syntax   (Lift)
import Lens.Micro.TH                (makeLenses)
import Lens.Micro
----------------------------------------------------------------------------------

data Expr b = Var Name
            | Con Tag Int -- ^ Con Tag Arity
            | Case (Expr b) [Alter b]
            | Lam [b] (Expr b)
            | Let Rec [Binding b] (Expr b)
            | App (Expr b) (Expr b)
            | Lit Lit
            deriving (Show, Eq, Read, Lift)

-- deriving instance (Eq b) => Eq (Expr b)

data Type = TyFun
          | TyVar Name
          | TyApp Type Type
          | TyCon Name
          deriving (Show, Read, Lift, Eq)

pattern TyInt :: Type
pattern TyInt = TyCon "Int#"

infixl 2 :$
pattern (:$) :: Expr b -> Expr b -> Expr b 
pattern f :$ x = App f x

infixl 2 :@
pattern (:@) :: Type -> Type -> Type
pattern f :@ x = TyApp f x

infixr 1 :->
pattern (:->) :: Type -> Type -> Type
pattern a :-> b = TyApp (TyApp TyFun a) b

{-# COMPLETE Binding :: Binding #-}
{-# COMPLETE (:=) :: Binding #-}
data Binding b = Binding b (Expr b)
    deriving (Show, Read, Eq, Lift)

infixl 1 :=
pattern (:=) :: b -> (Expr b) -> (Binding b)
pattern k := v = Binding k v

data Alter b = Alter AltCon [b] (Expr b)
    deriving (Show, Read, Eq, Lift)

data Rec = Rec
         | NonRec
         deriving (Show, Read, Eq, Lift)

data AltCon = AltData Tag
            | AltLit Lit
            | Default
            deriving (Show, Read, Eq, Lift)

data Lit = IntL Int
    deriving (Show, Read, Eq, Lift)

type Name = T.Text
type Tag = Int

data ScDef b = ScDef b [b] (Expr b)
    deriving (Show, Eq, Lift)

unliftScDef :: ScDef b -> Expr b
unliftScDef (ScDef _ as e) = Lam as e

data Module b = Module (Maybe (Name, [Name])) (Program b)
    deriving (Show, Lift)

data Program b = Program
    { _programScDefs   :: [ScDef b]
    , _programTypeSigs :: H.HashMap b Type
    }
    deriving (Show, Eq, Lift)

makeLenses ''Program
pure []

type Program' = Program Name
type Expr' = Expr Name
type ScDef' = ScDef Name
type Alter' = Alter Name
type Binding' = Binding Name

instance IsString (Expr b) where
    fromString = Var . fromString

instance IsString Type where
    fromString "" = error "IsString Type string may not be empty"
    fromString s
        | isUpper c     = TyCon . fromString $ s
        | otherwise     = TyVar . fromString $ s
        where (c:_) = s

instance (Hashable b) => Semigroup (Program b) where
    (<>) = undefined

instance (Hashable b) => Monoid (Program b) where
    mempty = Program mempty mempty

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

