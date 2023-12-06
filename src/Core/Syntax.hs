{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Core.Syntax
    ( Expr(..)
    , pattern (:$)
    , Binding(..)
    , pattern (:=)
    , Rec(..)
    , Alter(..)
    , Name
    , Tag
    , ScDef(..)
    , Module(..)
    , Program(..)
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

data Expr = Var Name
          | Con Tag Int -- Con Tag Arity
          | Let Rec [Binding] Expr
          | Case Expr [Alter]
          | Lam [Name] Expr
          | App Expr Expr
          | IntE Int
          deriving (Show, Read, Lift, Eq)

infixl 2 :$
pattern (:$) :: Expr -> Expr -> Expr
pattern f :$ x = App f x

{-# COMPLETE Binding :: Binding #-}
{-# COMPLETE (:=) :: Binding #-}
data Binding = Binding Name Expr
    deriving (Show, Read, Lift, Eq)

infixl 1 :=
pattern (:=) :: Name -> Expr -> Binding
pattern k := v = Binding k v

data Rec = Rec
         | NonRec
         deriving (Show, Read, Eq, Lift)

data Alter = Alter Tag [Name] Expr
    deriving (Show, Read, Lift, Eq)

type Name = String
type Tag = Int

data ScDef = ScDef Name [Name] Expr
    deriving (Show, Lift, Eq)

data Module = Module (Maybe (Name, [Name])) Program
    deriving (Show, Lift)

newtype Program = Program [ScDef]
    deriving (Show, Lift)

instance IsString Expr where
    fromString = Var

----------------------------------------------------------------------------------

instance Pretty Program where
    -- TODO: module header
    prettyPrec (Program ss) _ = mconcat $ intersperse "\n\n" $ fmap pretty ss

instance Pretty ScDef where
    prettyPrec (ScDef n as e) _ =
        mconcat (intersperse " " $ fmap IStr (n:as))
        <> " = " <> pretty e <> IBreak

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
    (<>) = coerce $ (<>) @[ScDef] 

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

-- TODO: export list awareness
insertModule :: Module -> Program -> Program
insertModule (Module _ m) p = p <> m

extractProgram :: Module -> Program
extractProgram (Module _ p) = p

