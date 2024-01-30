-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
  , TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances, ImpredicativeTypes #-}
module Rlp.Syntax
    (
    -- * AST
      RlpProgram(..)
    , Decl(..), Decl', RlpExpr(..), RlpExpr'
    , Pat(..), Pat'
    , Assoc(..)
    , Lit(..), Lit'
    , RlpType(..), RlpType'
    , ConAlt(..)
    , Binding(..), Binding'

    -- * Trees That Grow boilerplate
    -- ** Extension points
    , IdP, XRec, UnXRec(..), MapXRec(..)
    -- *** Decl
    , XFunD, XTySigD, XInfixD, XDataD, XXDeclD
    -- *** RlpExpr
    , XLetE, XVarE, XLamE, XCaseE, XIfE, XAppE, XLitE
    , XParE, XOAppE
    -- ** Pattern synonyms
    -- *** Decl
    , pattern FunD, pattern TySigD, pattern InfixD, pattern DataD
    -- *** RlpExpr
    , pattern LetE, pattern VarE, pattern LamE, pattern CaseE, pattern IfE
    , pattern AppE, pattern LitE, pattern ParE, pattern OAppE
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Classes
import Data.Kind                    (Type)
import Lens.Micro
import Lens.Micro.TH
import Core.Syntax                  hiding (Lit, Type, Binding, Binding')
import Core                         (HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

data RlpModule p = RlpModule
    { _rlpmodName       :: Text
    , _rlpmodProgram    :: RlpProgram p
    }

-- | dear god.
type PhaseShow p =
    ( Show (XRec p Pat), Show (XRec p RlpExpr)
    , Show (XRec p Lit), Show (IdP p)
    , Show (XRec p RlpType)
    , Show (XRec p Binding)
    )

newtype RlpProgram p = RlpProgram [Decl' p]

deriving instance (PhaseShow p, Show (XRec p Decl)) => Show (RlpProgram p)

data RlpType p = FunConT
               | FunT (RlpType' p) (RlpType' p)
               | AppT (RlpType' p) (RlpType' p)
               | VarT (IdP p)
               | ConT (IdP p)

type RlpType' p = XRec p RlpType

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
               => (IdP p) -> [Pat' p] -> (RlpExpr' p) -> (Maybe (Where p))
               -> Decl p
pattern TySigD :: (XTySigD p ~ ()) => [IdP p] -> (RlpType' p) -> Decl p
pattern DataD  :: (XDataD p ~ ()) => (IdP p) -> [IdP p] -> [ConAlt p] -> Decl p
pattern InfixD :: (XInfixD p ~ ()) => Assoc -> Int -> (IdP p) -> Decl p
pattern XDeclD :: (XXDeclD p ~ ()) => Decl p

pattern FunD n as e wh = FunD' () n as e wh
pattern TySigD ns t = TySigD' () ns t
pattern DataD n as cs = DataD' () n as cs
pattern InfixD a p n = InfixD' () a p n
pattern XDeclD = XDeclD' ()

type Decl' p = XRec p Decl

data Assoc = InfixL
           | InfixR
           | Infix
           deriving (Show)

data ConAlt p = ConAlt (IdP p) [RlpType' p]

deriving instance (Show (IdP p), Show (XRec p RlpType)) => Show (ConAlt p)

data RlpExpr p = LetE'  (XLetE p) [Binding' p] (RlpExpr' p)
               | VarE'  (XVarE p) (IdP p)
               | LamE'  (XLamE p) [Pat p] (RlpExpr' p)
               | CaseE' (XCaseE p) (RlpExpr' p) [(Alt p, Where p)]
               | IfE'   (XIfE p) (RlpExpr' p) (RlpExpr' p) (RlpExpr' p)
               | AppE'  (XAppE p) (RlpExpr' p) (RlpExpr' p)
               | LitE'  (XLitE p) (Lit p)
               | ParE'  (XParE p) (RlpExpr' p)
               | OAppE' (XOAppE p) (IdP p) (RlpExpr' p) (RlpExpr' p)
               | XRlpExprE' (XXRlpExprE p)

type family XLetE p
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
    ( Show (XLetE p), Show (XVarE p), Show (XLamE p)
    , Show (XCaseE p), Show (XIfE p), Show (XAppE p)
    , Show (XLitE p), Show (XParE p), Show (XOAppE p)
    , Show (XXRlpExprE p)
    , PhaseShow p
    ) => Show (RlpExpr p)

type RlpExpr' p = XRec p RlpExpr

class UnXRec p where
    unXRec :: XRec p f -> f p

class MapXRec p where
    mapXRec :: (f p -> f' p') -> XRec p f -> XRec p' f'

type family XRec p (f :: Type -> Type) = (r :: Type) | r -> p f

type family IdP p

type Where p = [Binding p]

-- do we want guards?
data Alt p = AltA (Pat' p) (RlpExpr' p)

deriving instance (PhaseShow p) => Show (Alt p)

data Binding p = PatB (Pat' p) (RlpExpr' p)
               | FunB (IdP p) [Pat' p] (RlpExpr' p)

type Binding' p = XRec p Binding

deriving instance (Show (XRec p Pat), Show (XRec p RlpExpr), Show (IdP p)
                  ) => Show (Binding p)

data Pat p = VarP (IdP p)
           | LitP (Lit' p)
           | ConP (IdP p) [Pat' p]

deriving instance (PhaseShow p) => Show (Pat p)

type Pat' p = XRec p Pat

data Lit p = IntL Int
           | CharL Char
           | ListL [RlpExpr' p]

deriving instance (PhaseShow p) => Show (Lit p)

type Lit' p = XRec p Lit

-- instance HasLHS Alt Alt Pat Pat where
--     _lhs = lens
--         (\ (AltA p _) -> p)
--         (\ (AltA _ e) p' -> AltA p' e)

-- instance HasRHS Alt Alt RlpExpr RlpExpr where
--     _rhs = lens
--         (\ (AltA _ e) -> e)
--         (\ (AltA p _) e' -> AltA p e')

makeBaseFunctor ''RlpExpr

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

