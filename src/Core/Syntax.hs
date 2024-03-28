{-|
Module      : Core.Syntax
Description : Core ASTs and the like
-}
{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
-- for recursion-schemes
{-# LANGUAGE DeriveTraversable, TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
module Core.Syntax
    (
    -- * Core AST
      ExprF(..), ExprF'
    , ScDef(..), ScDef'
    , Program(..), Program'
    , Type(..), Kind, pattern (:->), pattern TyInt
    , AlterF(..), Alter(..), Alter', AltCon(..)
    , pattern Binding, pattern Alter
    , Rec(..), Lit(..)
    , Pragma(..)
    -- ** Variables and identifiers
    , Name, Var(..), Tag, pattern (:^)
    , Binding, BindingF(..), pattern (:=), pattern (:$)
    , type Binding'
    -- ** Working with the fixed point of ExprF
    , Expr, Expr'
    , pattern Con, pattern Var, pattern App, pattern Lam, pattern Let
    , pattern Case, pattern Type, pattern Lit

    -- * pretty-printing
    , Out(out), WithTerseBinds(..)

    -- * Optics
    , HasArrowSyntax(..)
    , programScDefs, programTypeSigs, programDataTags, programTyCons
    , formalising, lambdaLifting
    , HasRHS(_rhs), HasLHS(_lhs)
    , _BindingF, _MkVar, _ScDef
    -- ** Classy optics
    , HasBinders(..), HasArrowStops(..), HasApplicants1(..), HasApplicants(..)
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
import Data.Hashable.Lifted
import Data.Foldable                (traverse_)
import Data.Functor
import Data.Monoid
import Data.Functor.Classes
import Data.Text                    qualified as T
import Data.Char
import Data.These
import Data.Aeson
import GHC.Generics                 ( Generic, Generic1
                                    , Generically(..), Generically1(..))
import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Kind                    qualified

import Data.Fix                     hiding (cata, ana)
import Data.Bifunctor               (Bifunctor(..))
import Data.Bifoldable              (bifoldr, Bifoldable(..))
import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)

-- Lift instances for the Core quasiquoters
import Misc
import Misc.Lift1
import Control.Lens
----------------------------------------------------------------------------------

data ExprF b a = VarF Name
               | ConF Tag Int -- ^ Con Tag Arity
               | CaseF a [AlterF b a]
               | LamF [b] a
               | LetF Rec [BindingF b a] a
               | AppF a a
               | LitF Lit
               | TypeF Type
               deriving (Functor, Foldable, Traversable)

type Expr b = Fix (ExprF b)

instance IsString (ExprF b a) where
    fromString = VarF . fromString

instance (IsString (f (Fix f))) => IsString (Fix f) where
    fromString = Fix . fromString

data Type = TyFun
          | TyVar Name
          | TyApp Type Type
          | TyCon Name
          | TyForall Var Type
          | TyKindType
          deriving (Show, Eq, Lift)

type Kind = Type

-- data TyCon = MkTyCon Name Kind
--     deriving (Eq, Show, Lift)

data Var = MkVar Name Type
    deriving (Eq, Show, Lift, Generic)

pattern (:^) :: Name -> Type -> Var
pattern n :^ t = MkVar n t

instance Hashable Var where
    hashWithSalt s (MkVar n _) = hashWithSalt s n

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
pattern TyInt = TyCon "Int#"

class HasArrowSyntax s a b | s -> a b where
    _arrowSyntax :: Prism' s (a, b)

instance HasArrowSyntax Type Type Type where
    _arrowSyntax = prism make unmake where
        make (s,t) = TyFun `TyApp` s `TyApp` t

        unmake (TyFun `TyApp` s `TyApp` t) = Right (s,t)
        unmake s                           = Left s

infixr 1 :->
pattern (:->) :: HasArrowSyntax s a b
              => a -> b -> s
-- pattern (:->) :: Type -> Type -> Type
pattern a :-> b <- (preview _arrowSyntax -> Just (a, b))
    where a :-> b = _arrowSyntax # (a, b)

data BindingF b a = BindingF b (ExprF b a)
    deriving (Functor, Foldable, Traversable)

type Binding b = BindingF b (Fix (ExprF b))

type Binding' = Binding Name

-- collapse = foldFix embed

pattern Binding :: b -> Expr b -> Binding b
pattern Binding k v <- BindingF k (wrapFix -> v)
    where Binding k v = BindingF k (unwrapFix v)

{-# COMPLETE (:=) #-}
{-# COMPLETE Binding #-}

infixl 1 :=
pattern (:=) :: b -> Expr b -> Binding b
pattern k := v = Binding k v

infixl 2 :$
pattern (:$) :: Expr b -> Expr b -> Expr b
pattern f :$ x = App f x

data AlterF b a = AlterF AltCon [b] (ExprF b a)
    deriving (Functor, Foldable, Traversable)

pattern Alter :: AltCon -> [b] -> Expr b -> Alter b
pattern Alter con bs e <- AlterF con bs (wrapFix -> e)
    where Alter con bs e = AlterF con bs (unwrapFix e)

type Alter b = AlterF b (Fix (ExprF b))

type Alter' = Alter Name

-- pattern Alter :: AltCon -> [b] -> Expr b -> Alter b
-- pattern Alter con bs e <- Fix (AlterF con bs (undefined -> e))
--     where Alter con bs e = Fix (AlterF con bs undefined)

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

-- unliftScDef :: ScDef b -> Expr b
-- unliftScDef (ScDef _ as e) = Lam as e

data Module b = Module (Maybe (Name, [Name])) (Program b)

data Program b = Program
    { _programScDefs   :: [ScDef b]
    , _programTypeSigs :: HashMap b Type
    , _programDataTags :: HashMap Name (Tag, Int)
    -- ^ map constructors to their tag and arity
    , _programTyCons   :: HashMap Name Kind
    -- ^ map type constructors to their kind
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
-- type Alter' = Alter Name
-- type Binding' = Binding Name

-- instance IsString (Expr b) where
--     fromString = Var . fromString

lambdaLifting :: Iso (ScDef b) (ScDef b') (b, Expr b) (b', Expr b')
lambdaLifting = iso sa bt where
    sa (ScDef n as e) = (n, e') where
        e' = Lam as e

    bt (n, Lam as e) = ScDef n as e
    bt (n, e)        = ScDef n [] e

----------------------------------------------------------------------------------

class HasRHS s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _rhs :: Lens s t a b

instance HasRHS (AlterF b a) (AlterF b a') (ExprF b a) (ExprF b a') where
    _rhs = lens
        (\ (AlterF _ _ e) -> e)
        (\ (AlterF t as _) e' -> AlterF t as e')

instance HasRHS (ScDef b) (ScDef b) (Expr b) (Expr b) where
    _rhs = lens
        (\ (ScDef _ _ e) -> e)
        (\ (ScDef n as _) e' -> ScDef n as e')

instance HasRHS (BindingF b a) (BindingF b' a') (ExprF b a) (ExprF b' a')

class HasLHS s t a b | s -> a, t -> b, s b -> t, t a -> s where
    _lhs :: Lens s t a b

instance HasLHS (ScDef b) (ScDef b) (b, [b]) (b, [b]) where
    _lhs = lens
        (\ (ScDef n as _) -> (n,as))
        (\ (ScDef _ _ e) (n',as') -> ScDef n' as' e)

-- instance HasLHS (Binding b) (Binding b) b b where
    -- _lhs = lens
    --     (\ (k := _) -> k)
    --     (\ (_ := e) k' -> k' := e)

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
        Lam [b] e      -> LamF [b] e
        Lam (b:bs) e   -> LamF [b] (Lam bs e)
        Let r (b:bs) e -> LetF r [b] (Let r bs e)
        x              -> project x

    bt :: Expr b -> Expr b
    bt = cata \case
        LamF [b] (Lam bs e)                -> Lam (b:bs) e
        LetF r [b] (Let r' bs e) | r == r' -> Let r (b:bs) e
        x                                  -> embed x

--------------------------------------------------------------------------------

newtype WithTerseBinds a = WithTerseBinds a

class MakeTerse a where
    type AsTerse a :: Data.Kind.Type
    asTerse :: a -> AsTerse a

instance MakeTerse Var where
    type AsTerse Var = Name
    asTerse (MkVar n _) = n

instance (Hashable b, Out b, Out (AsTerse b), MakeTerse b)
      => Out (WithTerseBinds (Program b)) where
    out (WithTerseBinds p)
            = vsep [ (datatags <> "\n")
                   , defs ]
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

            prettyGroup :: These (b, Type) (ScDef b) -> Doc ann
            prettyGroup = bifoldr vs vs mempty
                        . bimap (uncurry prettyTySig')
                                (out . WithTerseBinds)
                where vs a b = a <> ";" <> line <> b

            cataDataTag n (t,a) acc = prettyDataTag n t a <> line <> acc

instance (Hashable b, Out b) => Out (Program b) where
    out p = vsep [ datatags <> "\n"
                 , defs ]
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

            prettyGroup :: These (b, Type) (ScDef b) -> Doc ann
            prettyGroup = bifoldr vs vs mempty
                        . bimap (uncurry prettyTySig) out
                where vs a b = a <> ";" <> line <> b

            cataDataTag n (t,a) acc = prettyDataTag n t a <> line <> acc

unionThese :: These a b -> These a b -> These a b
unionThese (This a) (That b) = These a b
unionThese (That b) (This a) = These a b
unionThese (These a b) _     = These a b

prettyDataTag :: (Out n, Out t, Out a)
              => n -> t -> a -> Doc ann
prettyDataTag n t a =
    hsep ["{-#", "PackData", ttext n, ttext t, ttext a, "#-}"]

prettyTySig :: (Out n, Out t) => n -> t -> Doc ann
prettyTySig n t = hsep [ttext n, ":", out t]

prettyTySig' :: (MakeTerse n, Out (AsTerse n), Out t) => n -> t -> Doc ann
prettyTySig' n t = hsep [ttext (asTerse n), ":", out t]

-- out Type
-- TyApp | appPrec   | left
-- (:->) | appPrec-1 | right

instance Out Type where
    outPrec _ (TyVar n)   = ttext n
    outPrec _ TyFun       = "(->)"
    outPrec _ (TyCon n)   = ttext n
    outPrec p (a :-> b)   = maybeParens (p>appPrec-1) $
        hsep [outPrec appPrec a, "->", outPrec (appPrec-1) b]
    outPrec p (TyApp f x) = maybeParens (p>appPrec) $
        outPrec appPrec f <+> outPrec appPrec1 x
    outPrec p (TyForall a m) = maybeParens (p>appPrec-2) $
        "∀" <+> (outPrec appPrec1 a <> ".") <+> out m
    outPrec _ TyKindType = "Type"

instance (Out b, Out (AsTerse b), MakeTerse b)
      => Out (WithTerseBinds (ScDef b)) where
    out (WithTerseBinds sc) = hsep [name, as, "=", hang 1 e]
        where
            name = ttext $ sc ^. _lhs . _1 . to asTerse
            as = sc & hsepOf (_lhs . _2 . each . to asTerse . to ttext)
            e = out $ sc ^. _rhs

instance (Out b) => Out (ScDef b) where
    out sc = hsep [name, as, "=", hang 1 e]
        where
            name = ttext $ sc ^. _lhs . _1
            as = sc & hsepOf (_lhs . _2 . each . to ttext)
            e = out $ sc ^. _rhs

-- out Expr
-- LamF | appPrec1 | right
-- AppF | appPrec  | left

instance (Out b, Out a) => Out (ExprF b a) where
    outPrec = outPrec1

instance (Out b) => Out1 (ExprF b) where
    liftOutPrec pr _ (VarF n) = ttext n
    liftOutPrec pr _ (ConF t a) = "Pack{" <> (ttext t <+> ttext a) <> "}"
    liftOutPrec pr p (LamF bs e) = maybeParens (p>0) $
        hsep ["λ", hsep (outPrec appPrec1 <$> bs), "->", pr 0 e]
    liftOutPrec pr p (LetF r bs e) = maybeParens (p>0)
                               $ vsep [ hsep [out r, bs']
                                      , hsep ["in", pr 0 e] ]
        where bs' = liftExplicitLayout (liftOutPrec pr 0) bs
    liftOutPrec pr p (AppF f x) = maybeParens (p>appPrec) $
        pr appPrec f <+> pr appPrec1 x
    liftOutPrec pr p (LitF l) = outPrec p l
    liftOutPrec pr p (CaseF e as) = maybeParens (p>0) $
            vsep [ "case" <+> pr 0 e <+> "of"
                 , nest 2 as' ]
        where as' = liftExplicitLayout (liftOutPrec pr 0) as
    liftOutPrec pr p (TypeF t) = "@" <> outPrec appPrec1 t

instance Out Rec where
    out Rec = "letrec"
    out NonRec = "let"

instance (Out b, Out a) => Out (AlterF b a) where
    outPrec = outPrec1

instance (Out b) => Out1 (AlterF b) where
    liftOutPrec pr _ (AlterF c as e) =
        hsep [out c, hsep (out <$> as), "->", liftOutPrec pr 0 e]

instance Out AltCon where
    out (AltData n) = ttext n
    out (AltLit l) = out l
    out (AltTag t) = "<" <> ttext t <> ">"
    out AltDefault = "_"

instance Out Lit where
    out (IntL n) = ttext n

instance (Out b, Out a) => Out (BindingF b a) where
    outPrec = outPrec1

instance Out b => Out1 (BindingF b) where
    liftOutPrec pr _ (BindingF k v) = hsep [out k, "=", liftOutPrec pr 0 v]

liftExplicitLayout :: (a -> Doc ann) -> [a] -> Doc ann
liftExplicitLayout pr as = vcat inner <+> "}" where
    inner = zipWith (<+>) delims (pr <$> as)
    delims = "{" : repeat ";"

explicitLayout :: (Out a) => [a] -> Doc ann
explicitLayout as = vcat inner <+> "}" where
    inner = zipWith (<+>) delims (out <$> as)
    delims = "{" : repeat ";"

instance Out Var where
    outPrec p (MkVar n t) = maybeParens (p>0) $
        hsep [out n, ":", out t]

--------------------------------------------------------------------------------

-- instance Functor Alter where
--     fmap f (Alter con bs e) = Alter con (f <$> bs) e'
--         where
--             e' = foldFix (embed . bimap' f id) e
--             bimap' = $(makeBimap ''ExprF)

-- instance Foldable Alter where
-- instance Traversable Alter where
-- instance Functor Binding where
-- instance Foldable Binding where
-- instance Traversable Binding where

liftShowsPrecExpr :: (Show b)
                  => (Int -> a -> ShowS)
                  -> ([a] -> ShowS)
                  -> Int -> ExprF b a -> ShowS
liftShowsPrecExpr = $(makeLiftShowsPrec ''ExprF)

showsPrec1Expr :: (Show b, Show a)
               => Int -> ExprF b a -> ShowS
showsPrec1Expr = $(makeShowsPrec1 ''ExprF)

instance (Show b) => Show1 (AlterF b) where
    liftShowsPrec sp spl d (AlterF con bs e) =
        showsTernaryWith showsPrec showsPrec (liftShowsPrecExpr sp spl)
                         "AlterF" d con bs e

instance (Show b) => Show1 (BindingF b) where
    liftShowsPrec sp spl d (BindingF k v) =
        showsBinaryWith showsPrec (liftShowsPrecExpr sp spl)
                        "BindingF" d k v

instance (Show b, Show a) => Show (BindingF b a) where
    showsPrec d (BindingF k v)
        = showParen (d > 10)
        $ showString "BindingF" . showChar ' '
        . showsPrec 11 k . showChar ' '
        . showsPrec1Expr 11 v

instance (Show b, Show a) => Show (AlterF b a) where
    showsPrec d (AlterF con bs e)
        = showParen (d > 10)
        $ showString "AlterF" . showChar ' '
        . showsPrec 11 con . showChar ' '
        . showsPrec 11 bs . showChar ' '
        . showsPrec1Expr 11 e

deriveShow1 ''ExprF

deriving instance (Show b, Show a) => Show (ExprF b a)
-- deriving instance (Show b, Show a) => Show (BindingF b a)
-- deriving instance (Show b, Show a) => Show (AlterF b a)
deriving instance Show b => Show (ScDef b)
deriving instance Show b => Show (Program b)

bimapExpr :: (b -> b') -> (a -> a')
          -> ExprF b a -> ExprF b' a'
bimapExpr = $(makeBimap ''ExprF)

bifoldrExpr :: (b -> c -> c)
            -> (a -> c -> c)
            -> c -> ExprF b a -> c
bifoldrExpr = $(makeBifoldr ''ExprF)

bitraverseExpr :: Applicative f
               => (b -> f b')
               -> (a -> f a')
               -> ExprF b a -> f (ExprF b' a')
bitraverseExpr = $(makeBitraverse ''ExprF)

instance Bifunctor AlterF where
    bimap f g (AlterF con bs e) = AlterF con (f <$> bs) (bimapExpr f g e)

instance Bifunctor BindingF where
    bimap f g (BindingF k v) = BindingF (f k) (bimapExpr f g v)

instance Bifoldable AlterF where
    bifoldr f g z (AlterF con bs e) = bifoldrExpr f g z' e where
        z' = foldr f z bs

instance Bitraversable AlterF where
    bitraverse f g (AlterF con bs e) =
        AlterF con <$> traverse f bs <*> bitraverseExpr f g e

instance Bifoldable BindingF where
    bifoldr f g z (BindingF k v) = bifoldrExpr f g (f k z) v

instance Bitraversable BindingF where
    bitraverse f g (BindingF k v) =
        BindingF <$> f k <*> bitraverseExpr f g v

deriveBifunctor ''ExprF
deriveBifoldable ''ExprF
deriveBitraversable ''ExprF

instance Lift b => Lift1 (BindingF b) where
    liftLift lf (BindingF k v) = liftCon2 'BindingF (lift k) (liftLift lf v)

instance Lift b => Lift1 (AlterF b) where
    liftLift lf (AlterF con bs e) =
        liftCon3 'AlterF (lift con) (lift1 bs) (liftLift lf e)

instance Lift b => Lift1 (ExprF b) where
    liftLift lf (VarF k)      = liftCon 'VarF (lift k)
    liftLift lf (AppF f x)    = liftCon2 'AppF (lf f) (lf x)
    liftLift lf (LamF b e)    = liftCon2 'LamF (lift b) (lf e)
    liftLift lf (LetF r bs e) = liftCon3 'LetF (lift r) bs' (lf e)
        where bs' = liftLift (liftLift lf) bs
    liftLift lf (CaseF e as)  = liftCon2 'CaseF (lf e) as'
        where as' = liftLift (liftLift lf) as
    liftLift lf (TypeF t)     = liftCon 'TypeF (lift t)
    liftLift lf (LitF l)      = liftCon 'LitF (lift l)
    liftLift lf (ConF t a)    = liftCon2 'ConF (lift t) (lift a)

deriving instance (Lift b, Lift a) => Lift (ExprF b a)
deriving instance (Lift b, Lift a) => Lift (BindingF b a)
deriving instance (Lift b, Lift a) => Lift (AlterF b a)
deriving instance Lift b => Lift (ScDef b)
deriving instance Lift b => Lift (Program b)

--------------------------------------------------------------------------------

class HasApplicants1 s t a b | s -> a, t -> b, s b -> t, t a -> s where
    applicants1 :: Traversal s t a b

class HasApplicants s t a b | s -> a, t -> b, s b -> t, t a -> s where
    applicants :: Traversal s t a b

instance HasApplicants1 Type Type Type Type where
    applicants1 k (TyApp f x) = TyApp <$> applicants1 k f <*> k x
    applicants1 k x           = k x

instance HasApplicants Type Type Type Type where
    applicants k (TyApp f x) = TyApp <$> applicants k f <*> k x
    applicants k x           = pure x

-- instance HasArguments (ExprF b (Fix (ExprF b))) (ExprF b (Fix (ExprF b)))
--                        (Fix (ExprF b)) (Fix (ExprF b)) where
--     arguments k (AppF f x) = AppF <$> arguments k f <*> k x
--     arguments k x          = unwrapFix <$> k (wrapFix x)

-- instance HasArguments (f (Fix f)) (f (Fix f)) (Fix f) (Fix f)
--       => HasArguments (Fix f) (Fix f) (Fix f) (Fix f) where
--     arguments :: forall g. Applicative g
--                => LensLike' g (Fix f) (Fix f)
--     arguments k (Fix f) = Fix <$> arguments k f

-- arguments :: Traversal' (Expr b) (Expr b)
-- arguments k (App f x) = App <$> arguments k f <*> k x
-- arguments k x         = k x

class HasBinders s t a b | s -> a, t -> b, s b -> t, t a -> s where
    binders :: Traversal s t a b

instance HasBinders (ScDef b) (ScDef b') b b' where
    binders k (ScDef b as e) = ScDef <$> k b <*> traverse k as <*> binders k e

instance (Hashable b, Hashable b')
      => HasBinders (Program b) (Program b') b b' where
    binders :: forall f. (Applicative f)
            => LensLike f (Program b) (Program b') b b'
    binders k p
        = Program
        <$> traverse (binders k) (_programScDefs p)
        <*> (getAp . ifoldMap toSingleton $ _programTypeSigs p)
        <*> pure (_programDataTags p)
        <*> pure (_programTyCons p)
      where
        toSingleton :: b -> Type -> Ap f (HashMap b' Type)
        toSingleton b t = Ap $ (`H.singleton` t) <$> k b

instance HasBinders a a' b b'
    => HasBinders (ExprF b a) (ExprF b' a') b b' where
    binders :: forall f. (Applicative f)
                    => LensLike f (ExprF b a) (ExprF b' a') b b'
    binders k = go where
        go :: ExprF b a -> f (ExprF b' a')
        go (LamF bs e)   = LamF <$> traverse k bs <*> binders k e
        go (CaseF e as)  = CaseF <$> binders k e <*> eachbind as
        go (LetF r bs e) = LetF r <$> eachbind bs <*> binders k e
        go f             = bitraverse k (binders k) f

        eachbind :: forall p. Bitraversable p => [p b a] -> f [p b' a']
        eachbind bs = bitraverse k (binders k) `traverse` bs

instance HasBinders a a b b'
      => HasBinders (AlterF b a) (AlterF b' a) b b' where
    binders k (AlterF con bs e) =
        AlterF con <$> traverse k bs <*> traverseOf binders k e

instance HasBinders a a b b'
      => HasBinders (BindingF b a) (BindingF b' a) b b' where
    binders k (BindingF b v) = BindingF <$> k b <*> binders k v

instance (HasBinders (f b (Fix (f b))) (f b' (Fix (f b'))) b b')
      => HasBinders (Fix (f b)) (Fix (f b')) b b' where
    binders k (Fix f) = Fix <$> binders k f

class HasArrowStops s t a b | s -> a, t -> b, s b -> t, t a -> s where
    arrowStops :: Traversal s t a b

instance HasArrowStops Type Type Type Type where
    arrowStops k (s :-> t) = (:->) <$> k s <*> arrowStops k t
    arrowStops k t         = k t

--------------------------------------------------------------------------------

liftEqExpr :: (Eq b)
           => (a -> a' -> Bool)
           -> ExprF b a -> ExprF b a' -> Bool
liftEqExpr = $(makeLiftEq ''ExprF)

instance (Eq b, Eq a) => Eq (BindingF b a) where
    BindingF ka va == BindingF kb vb =
        ka == kb && va `eq` vb
      where eq = liftEqExpr (==)

instance (Eq b, Eq a) => Eq (AlterF b a) where
    AlterF cona bsa ea == AlterF conb bsb eb =
        cona == conb && bsa == bsb && ea `eq` eb
      where eq = liftEqExpr (==)

instance (Eq b) => Eq1 (AlterF b) where
    liftEq f (AlterF cona bsa ea) (AlterF conb bsb eb) =
        cona == conb && bsa == bsb && ea `eq` eb
      where eq = liftEqExpr f

instance (Eq b) => Eq1 (BindingF b) where
    liftEq f (BindingF ka va) (BindingF kb vb) =
        ka == kb && va `eq` vb
      where eq = liftEqExpr f

deriveEq1 ''ExprF

deriving instance (Eq b, Eq a) => Eq (ExprF b a)

makePrisms ''BindingF
makePrisms ''Var
makePrisms ''ScDef

deriving instance Generic (ExprF b a)
deriving instance Generic1 (ExprF b)
deriving instance Generic1 (AlterF b)
deriving instance Generic1 (BindingF b)
deriving instance Generic (AlterF b a)
deriving instance Generic (BindingF b a)
deriving instance Generic AltCon
deriving instance Generic Lit
deriving instance Generic Rec
deriving instance Generic Type

instance Hashable Lit
instance Hashable AltCon
instance Hashable Rec
instance Hashable Type
instance (Hashable b, Hashable a) => Hashable (BindingF b a)
instance (Hashable b, Hashable a) => Hashable (AlterF b a)
instance (Hashable b, Hashable a) => Hashable (ExprF b a)

instance Hashable b => Hashable1 (AlterF b)
instance Hashable b => Hashable1 (BindingF b)
instance Hashable b => Hashable1 (ExprF b)

deriving via (Generically Rec)
    instance ToJSON Rec
deriving via (Generically Lit)
    instance ToJSON Lit
deriving via (Generically AltCon)
    instance ToJSON AltCon
deriving via (Generically Type)
    instance ToJSON Type
deriving via (Generically Var)
    instance ToJSON Var

deriving via (Generically1 (BindingF b))
    instance ToJSON b => ToJSON1 (BindingF b)
deriving via (Generically1 (AlterF b))
    instance ToJSON b => ToJSON1 (AlterF b)
deriving via (Generically1 (ExprF b))
    instance ToJSON b => ToJSON1 (ExprF b)

