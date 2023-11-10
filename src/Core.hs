{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Core where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import Data.List            (intersperse)
----------------------------------------------------------------------------------

data Expr = Var Name
          | Con Int Int 
          | Let Rec [Binding] Expr
          | Case Expr [Alter]
          | Lam [Name] Expr
          | App Expr Expr
          | IntP Int
          deriving Show

infixl 2 :$
pattern (:$) :: Expr -> Expr -> Expr
pattern f :$ x = App f x

data Binding = Binding Name Expr
    deriving Show

infixl 1 :=
pattern (:=) :: Name -> Expr -> Binding
pattern k := v = Binding k v

data Rec = Rec
         | NonRec
         deriving (Show, Eq)

data Alter = Alter Int [Name] Expr
    deriving Show

type Name = String

data ScDef = ScDef Name [Name] Expr

newtype Program = Program [ScDef]

----------------------------------------------------------------------------------

instance Pretty Expr where
    prettyPrec _ (Var k)      = IStr k
    prettyPrec _ (IntP n)     = IStr $ show n
    prettyPrec _ (Con _ _)    = undefined
    prettyPrec _ (Let r bs e) =
        IStr (if r == Rec then "letrec " else "let ")
        <> binds <> IBreak
        <> "in " <> prettyPrec 0 e
        where
            binds = mconcat (fmap f (init bs))
                 <> IIndent (prettyPrec 0 $ last bs)
            f b = IIndent $ prettyPrec 0 b <> IBreak

instance Pretty Binding where
    prettyPrec _ (k := v) = IStr k <> " = " <> prettyPrec 0 v

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

