{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Rlp.AltSyntax
    (
    -- * AST
      Program(..), Decl(..), ExprF(..), Pat(..)
    , RlpExprF, RlpExpr
    , DataCon(..), Type(..)

    , Core.Name, PsName

    -- * Functor-related tools
    , Fix(..), Cofree(..), Sum(..), pattern Finl, pattern Finr
    )
    where
--------------------------------------------------------------------------------
import Data.Functor.Sum
import Control.Comonad.Cofree
import Data.Fix

import Text.Show.Deriving
import Data.Text                qualified as T
import Data.Pretty

import Compiler.Types
import Core.Syntax              qualified as Core
--------------------------------------------------------------------------------

type PsName = T.Text

newtype Program b a = Program [Decl b a]
    deriving Show

data Decl b a = FunD b [Pat b] a
              | DataD b [b] [DataCon b]
              deriving Show

data DataCon b = DataCon b [Type b]
    deriving Show

data Type b = VarT b
            | ConT b
            | AppT (Type b) (Type b)
            | FunT
            deriving Show

data ExprF b a = InfixEF b a a

-- type Expr b = Cofree (ExprF b)

type RlpExprF b = Sum (Core.ExprF b) (ExprF b)

type RlpExpr b = Fix (RlpExprF b)

data Pat b = VarP b
    deriving Show

deriveShow1 ''ExprF
deriving instance (Show b, Show a) => Show (ExprF b a)

pattern Finl :: f (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finl fa = Fix (InL fa)

pattern Finr :: g (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finr ga = Fix (InR ga)

--------------------------------------------------------------------------------

instance (Pretty b, Pretty a) => Pretty (ExprF b a) where
    prettyPrec = prettyPrec1

instance Pretty b => Pretty1 (ExprF b) where
    liftPrettyPrec pr p (InfixEF o a b) = maybeParens (p>0) $
        pr 1 a <+> pretty o <+> pr 1 b

instance (Pretty b, Pretty a) => Pretty (Decl b a) where
    prettyPrec = prettyPrec1

instance (Pretty b) => Pretty1 (Decl b) where
    liftPrettyPrec pr p (FunD f as e) = maybeParens (p>0) $
        hsep [ ttext f, hsep (prettyPrec appPrec1 <$> as)
             , "=", pr 0 e ]

instance (Pretty b) => Pretty (Pat b) where
    prettyPrec p (VarP b) = prettyPrec p b

instance (Pretty a, Pretty b) => Pretty (Program b a) where
    prettyPrec = prettyPrec1

instance (Pretty b) => Pretty1 (Program b) where
    liftPrettyPrec pr p (Program ds) = vsep $ liftPrettyPrec pr p <$> ds

