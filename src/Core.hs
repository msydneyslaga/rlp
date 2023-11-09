{-# LANGUAGE PatternSynonyms #-}
module Core where
----------------------------------------------------------------------------------
import Data.Coerce
----------------------------------------------------------------------------------

data Expr = Var Name
          | Con Int Int 
          | Let Rec [Binding] Expr
          | Case Expr [Alter]
          | Lam [Name] Expr
          | App Expr Expr
          | IntP Int

infixl 2 :$
pattern (:$) :: Expr -> Expr -> Expr
pattern f :$ x = App f x

data Binding = Binding Name Expr

infixl 1 :=
pattern (:=) :: Name -> Expr -> Binding
pattern k := v = Binding k v

data Rec = Rec
         | NonRec

data Alter = Alter Int [Name] Expr

type Name = String

data ScDef = ScDef Name [Name] Expr

newtype Program = Program [ScDef]

----------------------------------------------------------------------------------

instance Semigroup Program where
    (<>) = coerce $ (++) @ScDef

instance Monoid Program where
    mempty = Program []

----------------------------------------------------------------------------------

bindersOf :: [(Name, b)] -> [Name]
bindersOf = fmap fst

rhssOf :: [(Name, b)] -> [b]
rhssOf = fmap snd

isAtomic :: Expr -> Bool
isAtomic (Var _) = True
isAtomic _       = False

----------------------------------------------------------------------------------

corePrelude :: Program
corePrelude = Program
    [ ScDef "id" ["x"] (Var "x")
    , ScDef "K" ["x", "y"] (Var "x")
    , ScDef "K1" ["x", "y"] (Var "y")
    , ScDef "S" ["f", "g", "x"] (Var "f" :$ Var "x" :$ (Var "g" :$ Var "x"))
    , ScDef "compose" ["f", "g", "x"] (Var "f" :$ (Var "g" :$ Var "x"))
    , ScDef "twice" ["f", "g", "x"] (Var "f" :$ (Var "g" :$ Var "x"))
    ]

