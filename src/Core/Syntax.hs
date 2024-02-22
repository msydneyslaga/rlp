{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-- for recursion-schemes
{-# LANGUAGE DeriveTraversable, TypeFamilies #-}
module Core.Syntax
    (
    -- * Core AST
      ExprF(..), ExprF'
    , ScDef(..), ScDef'
    , Program(..), Program'
    , Type(..), Kind, pattern (:->), pattern TyInt
    , Alter(..), Alter', AltCon(..)
    , Rec(..), Lit(..)
    , Pragma(..)
    -- ** Variables and identifiers
    , Name, Var(..), TyCon(..), Tag
    , Binding(..), pattern (:=)
    , type Binding'
    -- ** Working with the fixed point of ExprF
    , Expr, Expr'
    , pattern Con, pattern Var, pattern App, pattern Lam, pattern Let
    , pattern Case, pattern Type, pattern Lit

    -- * Misc
    , Pretty(pretty)

    -- * Optics
    , programScDefs, programTypeSigs, programDataTags
    , formalising
    , HasRHS(_rhs), HasLHS(_lhs)
    )
    where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import Data.List                    (intersperse)
import Data.Function                ((&))
import Data.String
import Data.HashMap.Strict          (HashMap)
import Data.HashMap.Strict          qualified as H
import Data.Hashable
import Data.Text                    qualified as T
import Data.Char
import Data.These
import GHC.Generics                 (Generic, Generically(..))
import Text.Show.Deriving

import Data.Fix                     hiding (cata, ana)
import Data.Bifoldable              (bifoldr)
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)

-- Lift instances for the Core quasiquoters
import Misc.Lift1
import Control.Lens
----------------------------------------------------------------------------------

data ExprF b a = VarF Name
               | ConF Tag Int -- ^ Con Tag Arity
               | CaseF a [Alter b]
               | LamF [b] a
               | LetF Rec [Binding b] a
               | AppF a a
               | LitF Lit
               | TypeF Type
               deriving (Functor, Foldable, Traversable)

type Expr b = Fix (ExprF b)

data Type = TyFun
          | TyVar Var
          | TyApp Type Type
          | TyCon TyCon
          | TyForall Var Type
          | TyKindType
          | TyKindInferred
          deriving (Show, Eq, Lift)

type Kind = Type

data TyCon = MkTyCon Name Kind
    deriving (Eq, Show, Lift)

data Var = MkVar Name Type
    deriving (Eq, Show, Lift)

pattern Con :: Tag -> Int -> Expr b
pattern Con t a = Fix (ConF t a)

pattern Var :: Name -> Expr b
pattern Var b = Fix (VarF b)

pattern App :: Expr b -> Expr b -> Expr b
pattern App f x = Fix (AppF f x)

pattern Lam :: [b] -> Expr b -> Expr b
pattern Lam bs e = Fix (LamF bs e)

pattern Let :: Rec -> [Binding b] -> Expr b -> Expr b
pattern Let r bs e = Fix (LetF r bs e)

pattern Case :: Expr b -> [Alter b] -> Expr b
pattern Case e as = Fix (CaseF e as)

pattern Type :: Type -> Expr b
pattern Type t = Fix (TypeF t)

pattern Lit :: Lit -> Expr b
pattern Lit t = Fix (LitF t)

pattern TyInt :: Type
pattern TyInt = TyCon (MkTyCon "Int#" TyKindType)

infixr 1 :->
pattern (:->) :: Type -> Type -> Type
pattern a :-> b = TyApp (TyApp TyFun a) b

{-# COMPLETE Binding :: Binding #-}
{-# COMPLETE (:=) :: Binding #-}

data Binding b = Binding b (Expr b)

infixl 1 :=
pattern (:=) :: b -> Expr b -> Binding b
pattern k := v = Binding k v

data Alter b = Alter AltCon [b] (Expr b)

newtype Pragma = Pragma [T.Text]

data Rec = Rec
         | NonRec
         deriving (Show, Eq, Lift)

data AltCon = AltData Name
            | AltTag Tag
            | AltLit Lit
            | AltDefault
            deriving (Show, Eq, Lift)

newtype Lit = IntL Int
    deriving (Show, Eq, Lift)

type Name = T.Text
type Tag = Int

data ScDef b = ScDef b [b] (Expr b)

unliftScDef :: ScDef b -> Expr b
unliftScDef (ScDef _ as e) = Lam as e

data Module b = Module (Maybe (Name, [Name])) (Program b)

data Program b = Program
    { _programScDefs   :: [ScDef b]
    , _programTypeSigs :: HashMap b Type
    , _programDataTags :: HashMap b (Tag, Int)
    -- ^ map constructors to their tag and arity
    }
    deriving (Generic)
    deriving (Semigroup, Monoid)
        via Generically (Program b)

makeLenses ''Program
-- makeBaseFunctor ''Expr
pure []

-- this is a weird optic, stronger than Lens and Prism, but weaker than Iso.
programTypeSigsP :: (Hashable b) => Prism' (Program b) (HashMap b Type)
programTypeSigsP = prism
    (\b -> mempty & programTypeSigs .~ b)
    (Right . view programTypeSigs)

type ExprF' = ExprF Name

type Program' = Program Name
type Expr' = Expr Name
type ScDef' = ScDef Name
type Alter' = Alter Name
type Binding' = Binding Name

instance IsString (Expr b) where
    fromString = Var . fromString

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
        (\ (ScDef _ _ e) (n',as') -> ScDef n' as' e)

instance HasLHS (Binding b) (Binding b) b b where
    _lhs = lens
        (\ (k := _) -> k)
        (\ (_ := e) k' -> k' := e)

-- | This is not a valid isomorphism for expressions containing lambdas whose
-- bodies are themselves lambdas with multiple arguments:
--
-- >>> [coreExpr|\x -> \y z -> x|] ^. from (from formalising)
-- Lam ["x"] (Lam ["y"] (Lam ["z"] (Var "x")))
-- >>> [coreExpr|\x -> \y z -> x|]
-- Lam ["x"] (Lam ["y","z"] (Var "x"))
--
-- For this reason, it's best to consider 'formalising' as if it were two
-- unrelated unidirectional getters.

formalising :: Iso (Expr a) (Expr b) (Expr a) (Expr b)
formalising = iso sa bt where
    sa :: Expr a -> Expr a
    sa = ana \case
        Lam [b] e    -> LamF [b] e
        Lam (b:bs) e -> LamF [b] (Lam bs e)
        x            -> project x

    bt :: Expr b -> Expr b
    bt = cata \case
        LamF [b] (Lam bs e) -> Lam (b:bs) e
        x                   -> embed x

--------------------------------------------------------------------------------

instance (Hashable b, Pretty b) => Pretty (Program b) where
    pretty p = (datatags <> "\n")
           $+$ defs
        where
            datatags = ifoldrOf (programDataTags . ifolded) cataDataTag mempty p
            defs = vlinesOf (programJoinedDefs . to prettyGroup) p

            programJoinedDefs :: Fold (Program b) (These (b, Type) (ScDef b))
            programJoinedDefs = folding $ \p ->
                foldMapOf programTypeSigs thisTs p
                `u` foldMapOf programScDefs thatSc p
              where u = H.unionWith unionThese

            thisTs = ifoldMap @b @(HashMap b)
                (\n t -> H.singleton n (This (n,t)))
            thatSc = foldMap $ \sc ->
                H.singleton (sc ^. _lhs . _1) (That sc)

            prettyGroup :: These (b, Type) (ScDef b) -> Doc
            prettyGroup = bifoldr vcatWithSemi vcatWithSemi mempty
                        . bimap prettyTySig pretty

            vcatWithSemi a b = (a <+> ";") $$ b

            prettyTySig (n,t) = hsep [ttext n, "::", pretty t]

            unionThese (This a) (That b) = These a b
            unionThese (That b) (This a) = These a b
            unionThese (These a b) _     = These a b

            cataDataTag n (t,a) acc = prettyDataTag n t a $+$ acc

            prettyDataTag n t a =
                hsep ["{-#", "PackData", ttext n, ttext t, ttext a, "#-}"]

instance Pretty Type where
    prettyPrec _ (TyVar n) = ttext n
    prettyPrec _ TyFun = "(->)"
    prettyPrec _ (TyCon n) = ttext n
    prettyPrec p (a :-> b) = maybeParens (p>0) $
        hsep [prettyPrec 1 a, "->", prettyPrec 0 b]
    prettyPrec p (TyApp f x) = maybeParens (p>1) $
        prettyPrec 1 f <+> prettyPrec 2 x

instance (Pretty b) => Pretty (ScDef b) where
    pretty sc = hsep [name, as, "=", hang empty 1 e]
        where
            name = ttext $ sc ^. _lhs . _1
            as = sc & hsepOf (_lhs . _2 . each . to ttext)
            e = pretty $ sc ^. _rhs

instance (Pretty b) => Pretty (Expr b) where
    prettyPrec _ (Var n)      = ttext n
    prettyPrec _ (Con t a)    = "Pack{" <> (ttext t <+> ttext a) <> "}"
    prettyPrec _ (Lam bs e)   = hsep ["λ", hsep (prettyPrec 1 <$> bs), "->", pretty e]
    prettyPrec _ (Let r bs e) = hsep [word, explicitLayout bs]
                            $+$ hsep ["in", pretty e]
        where word = if r == Rec then "letrec" else "let"
    prettyPrec p (App f x)    = maybeParens (p>0) $
        prettyPrec 0 f <+> prettyPrec 1 x
    prettyPrec _ (Lit l)      = pretty l
    prettyPrec p (Case e as)  = maybeParens (p>0) $
        "case" <+> pretty e <+> "of"
        $+$ nest 2 (explicitLayout as)

instance (Pretty b) => Pretty (Alter b) where
    pretty (Alter c as e) =
        hsep [pretty c, hsep (pretty <$> as), "->", pretty e]

instance Pretty AltCon where
    pretty (AltData n) = ttext n
    pretty (AltLit l) = pretty l
    pretty (AltTag t) = ttext t
    pretty AltDefault = "_"

instance Pretty Lit where
    pretty (IntL n) = ttext n

instance (Pretty b) => Pretty (Binding b) where
    pretty (k := v) = hsep [pretty k, "=", pretty v]

explicitLayout :: (Pretty a) => [a] -> Doc
explicitLayout as = vcat inner <+> "}" where
    inner = zipWith (<+>) delims (pretty <$> as)
    delims = "{" : repeat ";"

instance Pretty TyCon
instance Pretty Var

--------------------------------------------------------------------------------

deriveShow1 ''ExprF

instance Lift b => Lift1 (ExprF b) where
    lift1 (VarF k)      = liftCon 'VarF (lift k)
    lift1 (AppF f x)    = liftCon2 'AppF (lift f) (lift x)
    lift1 (LamF b e)    = liftCon2 'LamF (lift b) (lift e)
    lift1 (LetF r bs e) = liftCon3 'LetF (lift r) (lift bs) (lift e)
    lift1 (CaseF e as)  = liftCon2 'CaseF (lift e) (lift as)
    lift1 (TypeF t)     = liftCon 'TypeF (lift t)
    lift1 (LitF l)      = liftCon 'LitF (lift l)

deriving instance (Show b, Show a) => Show (ExprF b a)
deriving instance Show b => Show (Binding b)
deriving instance Show b => Show (Alter b)

deriving instance Lift b => Lift (Binding b)
deriving instance Lift b => Lift (Alter b)

