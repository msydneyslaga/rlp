{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Rlp.AltSyntax
    (
    -- * AST
      Program(..), Decl(..), ExprF(..), Pat(..)
    , RlpExprF, RlpExpr, Binding(..), Alter(..)
    , DataCon(..), Type(..)

    , Core.Name, PsName
    , pattern (Core.:->)

    -- * Optics
    , programDecls
    , _VarP, _FunB, _VarB

    -- * Functor-related tools
    , Fix(..), Cofree(..), Sum(..), pattern Finl, pattern Finr
    )
    where
--------------------------------------------------------------------------------
import Data.Functor.Sum
import Control.Comonad.Cofree
import Data.Fix
import Data.Function            (fix)
import Control.Lens

import Text.Show.Deriving
import Data.Text                qualified as T
import Data.Pretty

import Compiler.Types
import Core.Syntax              qualified as Core
--------------------------------------------------------------------------------

type PsName = T.Text

newtype Program b a = Program [Decl b a]
    deriving Show

programDecls :: Lens' (Program b a) [Decl b a]
programDecls = lens (\ (Program ds) -> ds) (const Program)

data Decl b a = FunD b [Pat b] a
              | DataD b [b] [DataCon b]
              | TySigD b (Type b)
              deriving Show

data DataCon b = DataCon b [Type b]
    deriving Show

data Type b = VarT b
            | ConT b
            | AppT (Type b) (Type b)
            | FunT
            deriving Show

instance Core.HasArrowSyntax (Type b) (Type b) (Type b) where
    _arrowSyntax = prism make unmake where
        make (s,t) = FunT `AppT` s `AppT` t

        unmake (FunT `AppT` s `AppT` t) = Right (s,t)
        unmake s                        = Left s

data ExprF b a = InfixEF b a a
               | LetEF Core.Rec [Binding b a] a
               | CaseEF a [Alter b a]
               deriving (Functor, Foldable, Traversable)

data Alter b a = Alter (Pat b) a
               deriving (Show, Functor, Foldable, Traversable)

data Binding b a = FunB b [Pat b] a
                 | VarB (Pat b) a
                 deriving (Show, Functor, Foldable, Traversable)

-- type Expr b = Cofree (ExprF b)

type RlpExprF b = Sum (Core.ExprF b) (ExprF b)

type RlpExpr b = Fix (RlpExprF b)

data Pat b = VarP b
           | ConP b
           | AppP (Pat b) (Pat b)
           deriving Show

deriveShow1 ''Alter
deriveShow1 ''Binding
deriveShow1 ''ExprF
deriving instance (Show b, Show a) => Show (ExprF b a)

pattern Finl :: f (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finl fa = Fix (InL fa)

pattern Finr :: g (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finr ga = Fix (InR ga)

--------------------------------------------------------------------------------

instance (Pretty b, Pretty a) => Pretty (ExprF b a) where
    prettyPrec = prettyPrec1

instance (Pretty b, Pretty a) => Pretty (Alter b a) where
    prettyPrec = prettyPrec1

instance (Pretty b) => Pretty1 (Alter b) where
    liftPrettyPrec pr _ (Alter p e) =
        hsep [ pretty p, "->", pr 0 e]

instance Pretty b => Pretty1 (ExprF b) where
    liftPrettyPrec pr p (InfixEF o a b) = maybeParens (p>0) $
        pr 1 a <+> pretty o <+> pr 1 b
    liftPrettyPrec pr p (CaseEF e as) = maybeParens (p>0) $
        hsep [ "case", pr 0 e, "of" ]
        $+$ nest 2 (vcat $ liftPrettyPrec pr 0 <$> as)

instance (Pretty b, Pretty a) => Pretty (Decl b a) where
    prettyPrec = prettyPrec1

instance (Pretty b) => Pretty1 (Decl b) where
    liftPrettyPrec pr _ (FunD f as e) =
        hsep [ ttext f, hsep (prettyPrec appPrec1 <$> as)
             , "=", pr 0 e ]

    liftPrettyPrec _ _ (DataD f as []) =
        hsep [ "data", ttext f, hsep (pretty <$> as) ]

    liftPrettyPrec _ _ (DataD f as ds) =
        hsep [ "data", ttext f, hsep (pretty <$> as), cons ]
      where
        cons = vcat $ zipWith (<+>) delims (pretty <$> ds)
        delims = "=" : repeat "|"

    liftPrettyPrec _ _ (TySigD n t) =
        hsep [ ttext n, ":", pretty t ]

instance (Pretty b) => Pretty (DataCon b) where
    pretty (DataCon n as) = ttext n <+> hsep (prettyPrec appPrec1 <$> as)

-- (->) is given prec `appPrec-1`
instance (Pretty b) => Pretty (Type b) where
    prettyPrec _ (VarT n) = ttext n
    prettyPrec _ (ConT n) = ttext n
    prettyPrec p (s Core.:-> t) = maybeParens (p>appPrec-1) $
        hsep [ prettyPrec appPrec s, "->", prettyPrec (appPrec-1) t ]
    prettyPrec p (AppT f x) = maybeParens (p>appPrec) $
        prettyPrec appPrec f <+> prettyPrec appPrec1 x
    prettyPrec p FunT = maybeParens (p>0) "->"

instance (Pretty b) => Pretty (Pat b) where
    prettyPrec p (VarP b) = prettyPrec p b
    prettyPrec p (ConP b) = prettyPrec p b
    prettyPrec p (AppP c x) = maybeParens (p>appPrec) $
        prettyPrec appPrec c <+> prettyPrec appPrec1 x

instance (Pretty a, Pretty b) => Pretty (Program b a) where
    prettyPrec = prettyPrec1

instance (Pretty b) => Pretty1 (Program b) where
    liftPrettyPrec pr p (Program ds) = vsep $ liftPrettyPrec pr p <$> ds

makePrisms ''Pat
makePrisms ''Binding

