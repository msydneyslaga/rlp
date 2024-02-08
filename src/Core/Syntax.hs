{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies, DerivingVia #-}
-- for recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
  , TemplateHaskell, TypeFamilies #-}
module Core.Syntax
    ( Expr(..)
    , ExprF(..)
    , ExprF'(..)
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
    , Pragma(..)
    , unliftScDef
    , programScDefs
    , programTypeSigs
    , programDataTags
    , Expr'
    , ScDef'
    , Alter'
    , Binding'
    , HasRHS(_rhs)
    , HasLHS(_lhs)
    , Pretty(pretty)
    )
    where
----------------------------------------------------------------------------------
import Data.Coerce
import Data.Pretty
import Data.List                    (intersperse)
import Data.Function                ((&))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.String
import Data.HashMap.Strict          (HashMap)
import Data.HashMap.Strict          qualified as H
import Data.Hashable
import Data.Text                    qualified as T
import Data.Char
import Data.These
import Data.Bifoldable              (bifoldr)
import GHC.Generics                 (Generic, Generically(..))
-- Lift instances for the Core quasiquoters
import Language.Haskell.TH.Syntax   (Lift)
-- import Lens.Micro.TH                (makeLenses)
-- import Lens.Micro
import Control.Lens
----------------------------------------------------------------------------------

data Expr b = Var Name
            | Con Tag Int -- ^ Con Tag Arity
            | Case (Expr b) [Alter b]
            | Lam [b] (Expr b)
            | Let Rec [Binding b] (Expr b)
            | App (Expr b) (Expr b)
            | Lit Lit
            deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Expr b)

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
    deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Binding b)

infixl 1 :=
pattern (:=) :: b -> Expr b -> Binding b
pattern k := v = Binding k v

data Alter b = Alter AltCon [b] (Expr b)
    deriving (Show, Read, Lift)

deriving instance (Eq b) => Eq (Alter b)

newtype Pragma = Pragma [T.Text]

data Rec = Rec
         | NonRec
         deriving (Show, Read, Eq, Lift)

data AltCon = AltData Name
            | AltTag Tag
            | AltLit Lit
            | AltDefault
            deriving (Show, Read, Eq, Lift)

newtype Lit = IntL Int
    deriving (Show, Read, Eq, Lift)

type Name = T.Text
type Tag = Int

data ScDef b = ScDef b [b] (Expr b)
    deriving (Show, Lift)

unliftScDef :: ScDef b -> Expr b
unliftScDef (ScDef _ as e) = Lam as e

data Module b = Module (Maybe (Name, [Name])) (Program b)
    deriving (Show, Lift)

data Program b = Program
    { _programScDefs   :: [ScDef b]
    , _programTypeSigs :: HashMap b Type
    , _programDataTags :: HashMap b (Tag, Int)
    -- ^ map constructors to their tag and arity
    }
    deriving (Show, Lift, Generic)
    deriving (Semigroup, Monoid)
        via Generically (Program b)

makeLenses ''Program
makeBaseFunctor ''Expr
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

instance IsString Type where
    fromString "" = error "IsString Type string may not be empty"
    fromString s
        | isUpper c     = TyCon . fromString $ s
        | otherwise     = TyVar . fromString $ s
        where (c:_) = s

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

--------------------------------------------------------------------------------

-- TODO: print type sigs with corresponding scdefs
-- TODO: emit pragmas for datatags
instance (Hashable b, Pretty b) => Pretty (Program b) where
    pretty p = ifoldrOf (programDataTags . ifolded) cataDataTag mempty p
           $+$ vlinesOf (programJoinedDefs . to prettyGroup) p
        where
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
            prettyGroup = bifoldr ($$) ($$) mempty . bimap prettyTySig pretty

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
    prettyPrec p (TyApp f x) = maybeParens (p>0) $
        prettyPrec 0 f <+> prettyPrec 1 x

instance (Pretty b) => Pretty (ScDef b) where
    pretty sc = hsep [name, as, "=", hang empty 1 e, ";"]
        where
            name = ttext $ sc ^. _lhs . _1
            as = sc & hsepOf (_lhs . _2 . each . to ttext)
            e = pretty $ sc ^. _rhs

instance (Pretty b) => Pretty (Expr b) where
    prettyPrec _ (Var n)      = ttext n
    prettyPrec _ (Con t a)    = "Pack{" <> (ttext t <+> ttext a) <> "}"
    prettyPrec _ (Lam bs e)   = hsep ["λ", hsep (prettyPrec 1 <$> bs), "->", pretty e]
    prettyPrec _ (Let r bs e) = hsep [word, explicitLayout bs]
                             $$ hsep ["in", pretty e]
        where word = if r == Rec then "letrec" else "let"
    prettyPrec p (App f x)    = maybeParens (p>0) $
        prettyPrec 0 f <+> prettyPrec 1 x
    prettyPrec _ (Lit l)      = pretty l
    prettyPrec p (Case e as)  = maybeParens (p>0) $
        "case" <+> pretty e <+> "of"
        $$ nest 2 (explicitLayout as)

{-

x = pretty $ desugarRlpProg [rlpProg|
    main = 3
    data B = T | F
|]

-}

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

