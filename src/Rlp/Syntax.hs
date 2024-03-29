-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
  , TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
module Rlp.Syntax
    (
    -- * AST
      RlpProgram(..)
    , progDecls
    , Decl(..), Decl', RlpExpr(..), RlpExpr', RlpExprF(..)
    , Pat(..), Pat'
    , Alt(..), Where
    , Assoc(..)
    , Lit(..), Lit'
    , RlpType(..), RlpType'
    , ConAlt(..)
    , Binding(..), Binding'

    , _PatB, _FunB
    , _VarP, _LitP, _ConP

    -- * Trees That Grow boilerplate
    -- ** Extension points
    , IdP, IdP', XRec, UnXRec(..), MapXRec(..)
    -- *** Decl
    , XFunD, XTySigD, XInfixD, XDataD, XXDeclD
    -- *** RlpExpr
    , XLetE, XLetrecE, XVarE, XLamE, XCaseE, XIfE, XAppE, XLitE
    , XParE, XOAppE, XXRlpExprE
    -- ** Pattern synonyms
    -- *** Decl
    , pattern FunD, pattern TySigD, pattern InfixD, pattern DataD
    , pattern FunD'', pattern TySigD'', pattern InfixD'', pattern DataD''
    -- *** RlpExpr
    , pattern LetE, pattern LetrecE, pattern VarE, pattern LamE, pattern CaseE
    , pattern IfE , pattern AppE, pattern LitE, pattern ParE, pattern OAppE
    , pattern XRlpExprE
    -- *** RlpType
    , pattern FunConT'', pattern FunT'', pattern AppT'', pattern VarT''
    , pattern ConT''
    -- *** Pat
    , pattern VarP'', pattern LitP'', pattern ConP''
    -- *** Binding
    , pattern PatB''
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Classes
import Data.Functor.Identity
import Data.Kind                    (Type)
import GHC.Generics
import Language.Haskell.TH.Syntax   (Lift)
import Control.Lens
import Core.Syntax                  hiding (Lit, Type, Binding, Binding')
import Core                         (HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

data RlpModule p = RlpModule
    { _rlpmodName       :: Text
    , _rlpmodProgram    :: RlpProgram p
    }

-- | dear god.
type PhaseShow p =
    ( Show (XRec p (Pat p)), Show (XRec p (RlpExpr p))
    , Show (XRec p (Lit p)), Show (IdP p)
    , Show (XRec p (RlpType p))
    , Show (XRec p (Binding p))
    )

newtype RlpProgram p = RlpProgram [Decl' p]

progDecls :: Lens' (RlpProgram p) [Decl' p]
progDecls = lens
    (\ (RlpProgram ds) -> ds)
    (const RlpProgram)

deriving instance (PhaseShow p, Show (XRec p (Decl p))) => Show (RlpProgram p)

data RlpType p = FunConT
               | FunT (RlpType' p) (RlpType' p)
               | AppT (RlpType' p) (RlpType' p)
               | VarT (IdP p)
               | ConT (IdP p)

type RlpType' p = XRec p (RlpType p)

pattern FunConT'' :: (UnXRec p) => RlpType' p
pattern FunT''    :: (UnXRec p) => RlpType' p -> RlpType' p -> RlpType' p
pattern AppT''    :: (UnXRec p) => RlpType' p -> RlpType' p -> RlpType' p
pattern VarT''    :: (UnXRec p) => IdP p -> RlpType' p
pattern ConT''    :: (UnXRec p) => IdP p -> RlpType' p

pattern FunConT''     <- (unXRec -> FunConT)
pattern FunT''    s t <- (unXRec -> FunT s t)
pattern AppT''    s t <- (unXRec -> AppT s t)
pattern VarT''    n   <- (unXRec -> VarT n)
pattern ConT''    n   <- (unXRec -> ConT n)

deriving instance (PhaseShow p)
                  => Show (RlpType p)

data Decl p = FunD'   (XFunD p)    (IdP p) [Pat' p] (RlpExpr' p) (Maybe (Where p))
            | TySigD' (XTySigD p)  [IdP p] (RlpType' p)
            | DataD'  (XDataD p)   (IdP p) [IdP p] [ConAlt p]
            | InfixD' (XInfixD p)  Assoc Int (IdP p)
            | XDeclD' !(XXDeclD p)

deriving instance
    ( Show (XFunD p), Show (XTySigD p)
    , Show (XDataD p), Show (XInfixD p)
    , Show (XXDeclD p)
    , PhaseShow p
    ) 
    => Show (Decl p)

type family XFunD p
type family XTySigD p
type family XDataD p
type family XInfixD p
type family XXDeclD p

pattern FunD   :: (XFunD p ~ ())
               => IdP p -> [Pat' p] -> RlpExpr' p -> Maybe (Where p)
               -> Decl p
pattern TySigD :: (XTySigD p ~ ()) => [IdP p] -> RlpType' p -> Decl p
pattern DataD  :: (XDataD p ~ ()) => IdP p -> [IdP p] -> [ConAlt p] -> Decl p
pattern InfixD :: (XInfixD p ~ ()) => Assoc -> Int -> IdP p -> Decl p
pattern XDeclD :: (XXDeclD p ~ ()) => Decl p

pattern FunD n as e wh = FunD' () n as e wh
pattern TySigD ns t    = TySigD' () ns t
pattern DataD n as cs  = DataD' () n as cs
pattern InfixD a p n   = InfixD' () a p n
pattern XDeclD         = XDeclD' ()

pattern FunD''   :: (UnXRec p)
                 => IdP p -> [Pat' p] -> RlpExpr' p -> Maybe (Where p)
                 -> Decl' p
pattern TySigD'' :: (UnXRec p)
                 => [IdP p] -> RlpType' p -> Decl' p
pattern DataD''  :: (UnXRec p)
                 => IdP p -> [IdP p] -> [ConAlt p] -> Decl' p
pattern InfixD'' :: (UnXRec p)
                 => Assoc -> Int -> IdP p -> Decl' p

pattern FunD''   n as e wh <- (unXRec -> FunD' _ n as e wh)
pattern TySigD'' ns t      <- (unXRec -> TySigD' _ ns t)
pattern DataD''  n as ds   <- (unXRec -> DataD' _ n as ds)
pattern InfixD'' a p n     <- (unXRec -> InfixD' _ a p n)

type Decl' p = XRec p (Decl p)

data Assoc = InfixL
           | InfixR
           | Infix
           deriving (Show, Lift)

data ConAlt p = ConAlt (IdP p) [RlpType' p]

deriving instance (Show (IdP p), Show (XRec p (RlpType p))) => Show (ConAlt p)

data RlpExpr p = LetE'      (XLetE p) [Binding' p] (RlpExpr' p)
               | LetrecE'   (XLetrecE p) [Binding' p] (RlpExpr' p)
               | VarE'      (XVarE p) (IdP p)
               | LamE'      (XLamE p) [Pat p] (RlpExpr' p)
               | CaseE'     (XCaseE p) (RlpExpr' p) [(Alt p, Where p)]
               | IfE'       (XIfE p) (RlpExpr' p) (RlpExpr' p) (RlpExpr' p)
               | AppE'      (XAppE p) (RlpExpr' p) (RlpExpr' p)
               | LitE'      (XLitE p) (Lit p)
               | ParE'      (XParE p) (RlpExpr' p)
               | OAppE'     (XOAppE p) (IdP p) (RlpExpr' p) (RlpExpr' p)
               | XRlpExprE' !(XXRlpExprE p)
               deriving (Generic)

type family XLetE p
type family XLetrecE p
type family XVarE p
type family XLamE p
type family XCaseE p
type family XIfE p
type family XAppE p
type family XLitE p
type family XParE p
type family XOAppE p
type family XXRlpExprE p

pattern LetE :: (XLetE p ~ ()) => [Binding' p] -> RlpExpr' p -> RlpExpr p
pattern LetrecE :: (XLetrecE p ~ ()) => [Binding' p] -> RlpExpr' p -> RlpExpr p
pattern VarE :: (XVarE p ~ ()) => IdP p -> RlpExpr p
pattern LamE :: (XLamE p ~ ()) => [Pat p] -> RlpExpr' p -> RlpExpr p
pattern CaseE :: (XCaseE p ~ ()) => RlpExpr' p -> [(Alt p, Where p)] -> RlpExpr p
pattern IfE :: (XIfE p ~ ()) => RlpExpr' p -> RlpExpr' p -> RlpExpr' p -> RlpExpr p
pattern AppE :: (XAppE p ~ ()) => RlpExpr' p -> RlpExpr' p -> RlpExpr p
pattern LitE :: (XLitE p ~ ()) => Lit p -> RlpExpr p
pattern ParE :: (XParE p ~ ()) => RlpExpr' p -> RlpExpr p
pattern OAppE :: (XOAppE p ~ ()) => IdP p -> RlpExpr' p -> RlpExpr' p -> RlpExpr p
pattern XRlpExprE :: (XXRlpExprE p ~ ()) => RlpExpr p

pattern LetE bs e = LetE' () bs e
pattern LetrecE bs e = LetrecE' () bs e
pattern VarE n = VarE' () n
pattern LamE as e = LamE' () as e
pattern CaseE e as = CaseE' () e as
pattern IfE c a b = IfE' () c a b
pattern AppE f x = AppE' () f x
pattern LitE l = LitE' () l
pattern ParE e = ParE' () e
pattern OAppE n a b = OAppE' () n a b
pattern XRlpExprE = XRlpExprE' ()

deriving instance
    ( Show (XLetE p), Show (XLetrecE p), Show (XVarE p)
    , Show (XLamE p), Show (XCaseE p), Show (XIfE p)
    , Show (XAppE p), Show (XLitE p), Show (XParE p)
    , Show (XOAppE p), Show (XXRlpExprE p)
    , PhaseShow p
    ) => Show (RlpExpr p)

type RlpExpr' p = XRec p (RlpExpr p)

class UnXRec p where
    unXRec :: XRec p a -> a

class WrapXRec p where
    wrapXRec :: a -> XRec p a

class MapXRec p where
    mapXRec :: (a -> b) -> XRec p a -> XRec p b

-- old definition:
-- type family XRec p (f :: Type -> Type) = (r :: Type) | r -> p f
type family XRec p a = (r :: Type) | r -> p a

type family IdP p

type IdP' p = XRec p (IdP p)

type Where p = [Binding p]

-- do we want guards?
data Alt p = AltA (Pat' p) (RlpExpr' p)

deriving instance (PhaseShow p) => Show (Alt p)

data Binding p = PatB (Pat' p) (RlpExpr' p)
               | FunB (IdP p) [Pat' p] (RlpExpr' p)

type Binding' p = XRec p (Binding p)

pattern PatB'' :: (UnXRec p) => Pat' p -> RlpExpr' p -> Binding' p
pattern PatB'' p e <- (unXRec -> PatB p e)

deriving instance (Show (XRec p (Pat p)), Show (XRec p (RlpExpr p)), Show (IdP p)
                  ) => Show (Binding p)

data Pat p = VarP (IdP p)
           | LitP (Lit' p)
           | ConP (IdP p) [Pat' p]

pattern VarP'' :: (UnXRec p) => IdP p -> Pat' p
pattern LitP'' :: (UnXRec p) => Lit' p -> Pat' p
pattern ConP'' :: (UnXRec p) => IdP p -> [Pat' p] -> Pat' p

pattern VarP'' n    <- (unXRec -> VarP n)
pattern LitP'' l    <- (unXRec -> LitP l)
pattern ConP'' c as <- (unXRec -> ConP c as)

deriving instance (PhaseShow p) => Show (Pat p)

type Pat' p = XRec p (Pat p)

data Lit p = IntL Int
           | CharL Char
           | ListL [RlpExpr' p]

deriving instance (PhaseShow p) => Show (Lit p)

type Lit' p = XRec p (Lit p)

-- instance HasLHS Alt Alt Pat Pat where
--     _lhs = lens
--         (\ (AltA p _) -> p)
--         (\ (AltA _ e) p' -> AltA p' e)

-- instance HasRHS Alt Alt RlpExpr RlpExpr where
--     _rhs = lens
--         (\ (AltA _ e) -> e)
--         (\ (AltA p _) e' -> AltA p e')

-- makeBaseFunctor ''RlpExpr

-- showsTernaryWith :: (Int -> x -> ShowS)
--                  -> (Int -> y -> ShowS)
--                  -> (Int -> z -> ShowS)
--                  -> String -> Int
--                  -> x -> y -> z
--                  -> ShowS
-- showsTernaryWith sa sb sc name p a b c = showParen (p > 10)
--     $ showString name
--     . showChar ' ' . sa 11 a 
--     . showChar ' ' . sb 11 b
--     . showChar ' ' . sc 11 c

--------------------------------------------------------------------------------

makeLenses ''RlpModule
makePrisms ''Pat
makePrisms ''Binding

--------------------------------------------------------------------------------

data RlpExprF p a = LetE'F  (XLetE p) [Binding' p] a
                  | LetrecE'F  (XLetrecE p) [Binding' p] a
                  | VarE'F  (XVarE p) (IdP p)
                  | LamE'F  (XLamE p) [Pat p] a
                  | CaseE'F (XCaseE p) a [(Alt p, Where p)]
                  | IfE'F   (XIfE p) a a a
                  | AppE'F  (XAppE p) a a
                  | LitE'F  (XLitE p) (Lit p)
                  | ParE'F  (XParE p) a
                  | OAppE'F (XOAppE p) (IdP p) a a
                  | XRlpExprE'F !(XXRlpExprE p)
                  deriving (Functor, Foldable, Traversable, Generic)

type instance Base (RlpExpr p) = RlpExprF p

instance (UnXRec p) => Recursive (RlpExpr p) where
    project = \case
        LetE' xx bs e   -> LetE'F xx bs (unXRec e)
        LetrecE' xx bs e   -> LetrecE'F xx bs (unXRec e)
        VarE' xx n      -> VarE'F xx n
        LamE' xx ps e   -> LamE'F xx ps (unXRec e)
        CaseE' xx e as  -> CaseE'F xx (unXRec e) as
        IfE' xx a b c   -> IfE'F xx (unXRec a) (unXRec b) (unXRec c)
        AppE' xx f x    -> AppE'F xx (unXRec f) (unXRec x)
        LitE' xx l      -> LitE'F xx l
        ParE' xx e      -> ParE'F xx (unXRec e)
        OAppE' xx f a b -> OAppE'F xx f (unXRec a) (unXRec b)
        XRlpExprE' xx   -> XRlpExprE'F xx

instance (WrapXRec p) => Corecursive (RlpExpr p) where
    embed = \case
        LetE'F xx bs e   -> LetE' xx bs (wrapXRec e)
        LetrecE'F xx bs e   -> LetrecE' xx bs (wrapXRec e)
        VarE'F xx n      -> VarE' xx n
        LamE'F xx ps e   -> LamE' xx ps (wrapXRec e)
        CaseE'F xx e as  -> CaseE' xx (wrapXRec e) as
        IfE'F xx a b c   -> IfE' xx (wrapXRec a) (wrapXRec b) (wrapXRec c)
        AppE'F xx f x    -> AppE' xx (wrapXRec f) (wrapXRec x)
        LitE'F xx l      -> LitE' xx l
        ParE'F xx e      -> ParE' xx (wrapXRec e)
        OAppE'F xx f a b -> OAppE' xx f (wrapXRec a) (wrapXRec b)
        XRlpExprE'F xx   -> XRlpExprE' xx

