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

    -- * Trees That Grow extensions
    , UnXRec(..), MapXRec(..), XRec, IdP
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

data Decl p = FunD   (IdP p) [Pat' p] (RlpExpr' p) (Maybe (Where p))
            | TySigD [IdP p] (RlpType' p)
            | DataD  (IdP p) [IdP p] [ConAlt p]
            | InfixD Assoc Int (IdP p)

deriving instance (Show (IdP p), PhaseShow p) => Show (Decl p)

type Decl' p = XRec p Decl

data Assoc = InfixL
           | InfixR
           | Infix
           deriving (Show)

data ConAlt p = ConAlt (IdP p) [RlpType' p]

deriving instance (Show (IdP p), Show (XRec p RlpType)) => Show (ConAlt p)

data RlpExpr p = LetE  [Binding' p] (RlpExpr' p)
               | VarE  (IdP p)
               | LamE  [Pat p] (RlpExpr' p)
               | CaseE (RlpExpr' p) [(Alt p, Where p)]
               | IfE   (RlpExpr' p) (RlpExpr' p) (RlpExpr' p)
               | AppE  (RlpExpr' p) (RlpExpr' p)
               | LitE  (Lit p)
               | ParE  (RlpExpr' p)
               | OAppE (IdP p) (RlpExpr' p) (RlpExpr' p)

deriving instance (PhaseShow p) => Show (RlpExpr p)

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

