{-# LANGUAGE TemplateHaskell, PatternSynonyms #-}
module Rlp.AltSyntax
    (
    -- * AST
      Program(..), Decl(..), ExprF(..), Pat(..)
    , RlpExprF, RlpExpr, Binding(..), Alter(..)
    , DataCon(..), Type(..)
    , pattern IntT
    , Core.Rec(..)

    , AnnotatedRlpExpr, TypedRlpExpr
    , TypeF(..)

    , Core.Name, PsName
    , pattern (Core.:->)

    -- * Optics
    , programDecls
    , _VarP, _FunB, _VarB
    , _TySigD, _FunD

    -- * Functor-related tools
    , Fix(..), Cofree(..), Sum(..), pattern Finl, pattern Finr

    -- * Misc
    , serialiseCofree
    )
    where
--------------------------------------------------------------------------------
import Data.Functor.Sum
import Control.Comonad.Cofree
import Data.Fix                 hiding (cata)
import Data.Functor.Foldable
import Data.Function            (fix)
import GHC.Generics             ( Generic, Generic1
                                , Generically(..), Generically1(..))
import Data.Hashable
import Data.Hashable.Lifted
import GHC.Exts                 (IsString)
import Control.Lens             hiding ((.=))

import Data.Functor.Foldable.TH
import Text.Show.Deriving
import Data.Eq.Deriving
import Data.Text                qualified as T
import Data.Aeson
import Data.Pretty
import Misc.Lift1

import Compiler.Types
import Core.Syntax              qualified as Core
--------------------------------------------------------------------------------

type AnnotatedRlpExpr b = Cofree (RlpExprF b)

type TypedRlpExpr b = Cofree (RlpExprF b) (Type b)

type PsName = T.Text

newtype Program b a = Program [Decl b a]
    deriving (Show, Functor, Foldable, Traversable)

programDecls :: Lens' (Program b a) [Decl b a]
programDecls = lens (\ (Program ds) -> ds) (const Program)

data Decl b a = FunD b [Pat b] a
              | DataD Core.Name [Core.Name] [DataCon b]
              | TySigD Core.Name (Type b)
              deriving (Show, Functor, Foldable, Traversable)

data DataCon b = DataCon Core.Name [Type b]
    deriving (Show, Generic)

data Type b = VarT Core.Name
            | ConT Core.Name
            | AppT (Type b) (Type b)
            | FunT
            | ForallT b (Type b)
            deriving (Show, Eq, Generic, Functor, Foldable, Traversable)

instance (Hashable b) => Hashable (Type b)

pattern IntT :: (IsString b, Eq b) => Type b
pattern IntT = ConT "Int#"

instance Core.HasArrowSyntax (Type b) (Type b) (Type b) where
    _arrowSyntax = prism make unmake where
        make (s,t) = FunT `AppT` s `AppT` t

        unmake (FunT `AppT` s `AppT` t) = Right (s,t)
        unmake s                        = Left s

data ExprF b a = InfixEF b a a
               | LetEF Core.Rec [Binding b a] a
               | CaseEF a [Alter b a]
               deriving (Functor, Foldable, Traversable)
               deriving (Eq, Generic, Generic1)

data Alter b a = Alter (Pat b) a
               deriving (Show, Functor, Foldable, Traversable)
               deriving (Eq, Generic, Generic1)

data Binding b a = FunB b [Pat b] a
                 | VarB (Pat b) a
                 deriving (Show, Functor, Foldable, Traversable)
                 deriving (Eq, Generic, Generic1)

-- type Expr b = Cofree (ExprF b)

type RlpExprF b = Sum (Core.ExprF b) (ExprF b)

type RlpExpr b = Fix (RlpExprF b)

data Pat b = VarP b
           | ConP b
           | AppP (Pat b) (Pat b)
           deriving (Eq, Show, Generic, Generic1)

deriveShow1 ''Alter
deriveShow1 ''Binding
deriveShow1 ''ExprF
deriving instance (Show b, Show a) => Show (ExprF b a)

pattern Finl :: f (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finl fa = Fix (InL fa)

pattern Finr :: g (Fix (Sum f g)) -> Fix (Sum f g)
pattern Finr ga = Fix (InR ga)

--------------------------------------------------------------------------------

instance (Out b, Out a) => Out (ExprF b a) where
    outPrec = outPrec1

instance (Out b, Out a) => Out (Alter b a) where
    outPrec = outPrec1

instance (Out b) => Out1 (Alter b) where
    liftOutPrec pr _ (Alter p e) =
        hsep [ out p, "->", pr 0 e]

instance Out b => Out1 (ExprF b) where
    liftOutPrec pr p (InfixEF o a b) = maybeParens (p>0) $
        pr 1 a <+> out o <+> pr 1 b
    liftOutPrec pr p (CaseEF e as) = maybeParens (p>0) $
        vsep [ hsep [ "case", pr 0 e, "of" ]
             , nest 2 (vcat $ liftOutPrec pr 0 <$> as) ]

instance (Out b, Out a) => Out (Decl b a) where
    outPrec = outPrec1

instance (Out b) => Out1 (Decl b) where
    liftOutPrec pr _ (FunD f as e) =
        hsep [ ttext f, hsep (outPrec appPrec1 <$> as)
             , "=", pr 0 e ]

    liftOutPrec _ _ (DataD f as []) =
        hsep [ "data", ttext f, hsep (out <$> as) ]

    liftOutPrec _ _ (DataD f as ds) =
        hsep [ "data", ttext f, hsep (out <$> as), cons ]
      where
        cons = vcat $ zipWith (<+>) delims (out <$> ds)
        delims = "=" : repeat "|"

    liftOutPrec _ _ (TySigD n t) =
        hsep [ ttext n, ":", out t ]

instance (Out b) => Out (DataCon b) where
    out (DataCon n as) = ttext n <+> hsep (outPrec appPrec1 <$> as)

collapseForalls :: Prism' (Type b) ([b], Type b)
collapseForalls = prism' up down where
    up (bs,m) = foldr ForallT m bs
    down (ForallT x m) = case down m of
        Just (xs,m') -> Just (x : xs, m')
        Nothing      -> Just ([x],m)
    down _ = Nothing

-- (->) is given prec `appPrec-1`
instance (Out b) => Out (Type b) where
    outPrec _ (VarT n) = ttext n
    outPrec _ (ConT n) = ttext n
    outPrec p (s Core.:-> t) = maybeParens (p>arrPrec) $
        hsep [ outPrec arrPrec1 s, "->", outPrec arrPrec t ]
      where arrPrec = appPrec-1
            arrPrec1 = appPrec
    outPrec p (AppT f x) = maybeParens (p>appPrec) $
        outPrec appPrec f <+> outPrec appPrec1 x
    outPrec p FunT = maybeParens (p>0) "->"
    outPrec p t@(ForallT _ _) = maybeParens (p>0) $
        t ^. singular collapseForalls & \(bs,m) ->
            let bs' = "∀" <> (hsep $ outPrec appPrec1 <$> bs) <> "."
            in bs' <+> outPrec 0 m

instance (Out b) => Out (Pat b) where
    outPrec p (VarP b) = outPrec p b
    outPrec p (ConP b) = outPrec p b
    outPrec p (AppP c x) = maybeParens (p>appPrec) $
        outPrec appPrec c <+> outPrec appPrec1 x

instance (Out a, Out b) => Out (Program b a) where
    outPrec = outPrec1

instance (Out b) => Out1 (Program b) where
    liftOutPrec pr p (Program ds) = vsep $ liftOutPrec pr p <$> ds

makePrisms ''Pat
makePrisms ''Binding
makePrisms ''Decl

deriving instance (Lift b, Lift a) => Lift (Program b a)
deriving instance (Lift b, Lift a) => Lift (Decl b a)
deriving instance (Lift b) => Lift (Pat b)
deriving instance (Lift b) => Lift (DataCon b)
deriving instance (Lift b) => Lift (Type b)

instance Lift b => Lift1 (Binding b) where
    liftLift lf (VarB b a) = liftCon2 'VarB (lift b) (lf a)

instance Lift b => Lift1 (Alter b) where
    liftLift lf (Alter b a) = liftCon2 'Alter (lift b) (lf a)

instance Lift b => Lift1 (ExprF b) where
    liftLift lf (InfixEF o a b) =
        liftCon3 'InfixEF (lift o) (lf a) (lf b)
    liftLift lf (LetEF r bs e) =
        liftCon3 'LetEF (lift r) bs' (lf e)
      where bs' = liftLift (liftLift lf) bs
    liftLift lf (CaseEF e as) =
        liftCon2 'CaseEF (lf e) as'
      where as' = liftLift (liftLift lf) as

deriveEq1 ''Binding
deriveEq1 ''Alter
deriveEq1 ''ExprF

instance (Hashable b) => Hashable (Pat b)
instance (Hashable b, Hashable a) => Hashable (Binding b a)
instance (Hashable b, Hashable a) => Hashable (Alter b a)
instance (Hashable b, Hashable a) => Hashable (ExprF b a)
instance (Hashable b) => Hashable1 (Alter b)
instance (Hashable b) => Hashable1 (Binding b)
instance (Hashable b) => Hashable1 (ExprF b)

makeBaseFunctor ''Type

instance Core.HasArrowStops (Type b) (Type b) (Type b) (Type b) where
    arrowStops k (s Core.:-> t) = (Core.:->) <$> k s <*> Core.arrowStops k t
    arrowStops k t         = k t

deriving via (Generically1 Pat)
    instance ToJSON1 Pat
deriving via (Generically (Pat b))
    instance ToJSON b => ToJSON (Pat b)
deriving via (Generically1 (Alter b))
    instance ToJSON b => ToJSON1 (Alter b)
deriving via (Generically1 (Binding b))
    instance ToJSON b => ToJSON1 (Binding b)
deriving via (Generically1 (ExprF b))
    instance ToJSON b => ToJSON1 (ExprF b)
deriving via (Generically1 (RlpExprF b))
    instance ToJSON b => ToJSON1 (RlpExprF b)

serialiseCofree :: (Functor f, ToJSON1 f, ToJSON a) => Cofree f a -> Value
serialiseCofree = cata \case
    ann :<$ e -> object [ "ann" .= ann
                        , "val" .= toJSON1 e ]

