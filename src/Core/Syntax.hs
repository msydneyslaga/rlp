{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Core.Syntax
    ( Expr(..)
    , Id(..)
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
    , CoreProgram
    , bindersOf
    , rhssOf
    , isAtomic
    , insertModule
    , extractProgram
    )
    where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import Data.List                    (intersperse)
import Data.Function                ((&))
import Data.String
import Language.Haskell.TH.Syntax   (Lift)
----------------------------------------------------------------------------------

data Expr b = Var Id
            | Con Tag Int -- Con Tag Arity
            | Case (Expr b) [Alter b]
            | Lam [b] (Expr b)
            | Let Rec [Binding b] (Expr b)
            | App (Expr b) (Expr b)
            | LitE Literal
            deriving (Show, Read, Lift)

data Id = Name Name
    deriving (Show, Read, Lift)

infixl 2 :$
pattern (:$) :: Expr b  -> Expr b  -> Expr b 
pattern f :$ x = App f x

{-# COMPLETE Binding :: Binding #-}
{-# COMPLETE (:=) :: Binding #-}
data Binding b = Binding b (Expr b)
    deriving (Show, Read, Lift)

infixl 1 :=
pattern (:=) :: b -> (Expr b) -> (Binding b)
pattern k := v = Binding k v
data Alter b = Alter AltCon [b] (Expr b)
    deriving (Show, Read, Lift)

data Rec = Rec
         | NonRec
         deriving (Show, Read, Eq, Lift)

data AltCon = AltData Tag
            | AltLiteral Literal
            | Default
            deriving (Show, Read, Lift)

data Literal = IntL Int
    deriving (Show, Read, Lift)

type Name = String
type Tag = Int

data ScDef b = ScDef b [b] (Expr b)
    deriving (Show, Lift)

data Module b = Module (Maybe (Name, [Name])) (Program b)
    deriving (Show, Lift)

newtype Program b = Program [ScDef b]
    deriving (Show, Lift)

type CoreProgram = Program Name

instance IsString (Expr b) where
    fromString = Var . Name

----------------------------------------------------------------------------------

instance Semigroup (Program b) where
    (<>) = coerce $ (<>) @[ScDef b] 

instance Monoid (Program b) where
    mempty = Program []

----------------------------------------------------------------------------------

bindersOf :: [(Name, b)] -> [Name]
bindersOf = fmap fst

rhssOf :: [(Name, b)] -> [b]
rhssOf = fmap snd

isAtomic :: Expr b -> Bool
isAtomic (Var _)  = True
isAtomic (LitE _) = True
isAtomic _        = False

----------------------------------------------------------------------------------

-- TODO: export list awareness
insertModule :: (Module b) -> (Program b) -> (Program b)
insertModule (Module _ m) p = p <> m

extractProgram :: (Module b) -> (Program b)
extractProgram (Module _ p) = p

