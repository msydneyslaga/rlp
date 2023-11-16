{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Core.Syntax
    ( Expr(..)
    , pattern (:$)
    , Binding(..)
    , pattern (:=)
    , Rec(..)
    , Alter(..)
    , Name
    , ScDef(..)
    , Program(..)
    , corePrelude
    , bindersOf
    , rhssOf
    , isAtomic
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

data Expr = Var Name
          | Con Int Int -- Con Tag Arity
          | Let Rec [Binding] Expr
          | Case Expr [Alter]
          | Lam [Name] Expr
          | App Expr Expr
          | IntE Int
          deriving (Show, Lift)

infixl 2 :$
pattern (:$) :: Expr -> Expr -> Expr
pattern f :$ x = App f x

data Binding = Binding Name Expr
    deriving (Show, Lift)

infixl 1 :=
pattern (:=) :: Name -> Expr -> Binding
pattern k := v = Binding k v

data Rec = Rec
         | NonRec
         deriving (Show, Eq, Lift)

data Alter = Alter Int [Name] Expr
    deriving (Show, Lift)

type Name = String

data ScDef = ScDef Name [Name] Expr
    deriving (Show, Lift)

newtype Program = Program [ScDef]
    deriving (Show, Lift)

instance IsString Expr where
    fromString = Var

----------------------------------------------------------------------------------

instance Pretty Expr where
    prettyPrec (Var k)      = withPrec maxBound $ IStr k
    prettyPrec (IntE n)     = withPrec maxBound $ iShow n
    prettyPrec (Con t a)    = withPrec maxBound $
        "Pack{" <> iShow t <> " " <> iShow a <> "}"
    prettyPrec (Let r bs e) = withPrec 0 $
        IStr (if r == Rec then "letrec " else "let ")
        <> binds <> IBreak
        <> "in " <> pretty e
        where
            binds = mconcat (f <$> init bs)
                 <> IIndent (pretty $ last bs)
            f b = IIndent $ pretty b <> IBreak
    prettyPrec (Lam ns e)   = withPrec 0 $ 
        IStr "λ" <> binds <> " -> " <> pretty e
        where
            binds = fmap IStr ns & intersperse " " & mconcat
    prettyPrec (Case e as)   = withPrec 0 $
            "case " <> IIndent (pretty e <> " of" <> IBreak <> alts)
        where
            -- TODO: don't break on last alt
            alts = mconcat $ fmap palt as
            palt x = IIndent $ pretty x <> IBreak
    prettyPrec (App f x) = \p -> bracketPrec 0 p $
        case f of
            -- application is left-associative; don't increase prec if the
            -- expression being applied is itself an application
            (_:$_) -> precPretty       p  f <> " " <> precPretty (succ p) x
            _      -> precPretty (succ p) f <> " " <> precPretty (succ p) x

instance Pretty Alter where
    prettyPrec (Alter t bs e) = withPrec 0 $
            "<" <> IStr (show t) <> "> " <> binds <> " -> " <> pretty e
        where
            binds = mconcat $ intersperse " " (fmap IStr bs)

instance Pretty Binding where
    prettyPrec (k := v) = withPrec 0 $ IStr k <> " = " <> precPretty 0 v

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
    , ScDef "twice" ["f", "x"] (Var "f" :$ (Var "f" :$ Var "x"))
    , ScDef "False" [] $ Con 0 0
    , ScDef "True" [] $ Con 1 0
    ]

