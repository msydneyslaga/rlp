{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Core where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import Data.List            (intersperse)
import Data.Function        ((&))
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
            binds = mconcat (f <$> init bs)
                 <> IIndent (prettyPrec 0 $ last bs)
            f b = IIndent $ prettyPrec 0 b <> IBreak
    prettyPrec p (Lam ns e)
        | p > 0               = iBracket l
        | otherwise           =          l
        where
            l = IStr "λ" <> binds <> " -> " <> prettyPrec 0 e
            binds = fmap IStr ns & intersperse " " & mconcat
    prettyPrec p (Case e as)
        | p > 0               = iBracket c
        | otherwise           =          c
        where
            c = "case " <> IIndent (prettyPrec 0 e <> " of" <> IBreak <> alts)
            -- TODO: don't break on last alt
            alts = mconcat $ fmap palt as
            palt x = IIndent $ prettyPrec 0 x <> IBreak
    prettyPrec p (App f x)
        | p > 0               = iBracket a
        | otherwise           =          a
        where
            a = case f of
                -- application is left-associative; don't increase prec if the
                -- expression being applied is itself an application
                (_:$_) -> prettyPrec       p  f <> " " <> prettyPrec (succ p) x
                _      -> prettyPrec (succ p) f <> " " <> prettyPrec (succ p) x

instance Pretty Alter where
    prettyPrec p (Alter t bs e)
        | p > 0     = iBracket a
        | otherwise =          a
        where
            a = "<" <> IStr (show t) <> "> " <> binds <> " -> " <> prettyPrec 0 e
            binds = mconcat $ intersperse " " (fmap IStr bs)

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

