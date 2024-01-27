-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable
  , TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE TypeFamilies, TypeFamilyDependencies #-}
module Rlp.Syntax
    (
    -- * AST
      RlpProgram(..)
    , Decl(..), Decl', RlpExpr(..), RlpExpr'
    , Pat(..), Pat'
    , Assoc(..)
    , Lit(..), Lit'
    , Type(..)
    , pattern (:->)
    , ConAlt(..)

    -- * Pattern synonyms for unused extensions
    -- ** Decl
    , pattern InfixD', pattern FunD'
    -- ** RlpExpr
    , pattern ParE', pattern VarE', pattern LitE'

    -- * Trees That Grow extensions
    , UnXRec(..), MapXRec(..), XRec, IdP
    -- ** RlpExpr
    , XLetE, XVarE, XConE, XLamE, XCaseE, XIfE, XAppE, XLitE, XXRlpExpr
    -- ** Decl
    , XFunD, XTySigD, XDataD, XInfixD, XXDecl
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Classes
import Lens.Micro
import Lens.Micro.TH
import Core.Syntax                  hiding (Lit)
import Core                         (HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

data RlpModule p = RlpModule
    { _rlpmodName       :: Text
    , _rlpmodProgram    :: RlpProgram p
    }

newtype RlpProgram p = RlpProgram [Decl p]

data Decl p = FunD   (XFunD p)   (IdP p) [Pat' p] (RlpExpr' p) (Maybe (Where p))
            | TySigD (XTySigD p) [IdP p] Type
            | DataD  (XDataD p)  (IdP p) [IdP p] [ConAlt p]
            | InfixD (XInfixD p) Assoc Int (IdP p)
            | XDecl  !(XXDecl p)

type family XFunD p
type family XTySigD p
type family XDataD p
type family XInfixD p
type family XXDecl p

pattern FunD' :: (XFunD p ~ ())
              => IdP p -> [Pat' p] -> RlpExpr' p -> (Maybe (Where p))
              -> Decl p
pattern FunD' n as e wh = FunD () n as e wh

pattern InfixD' :: (XInfixD p ~ ()) => Assoc -> Int -> (IdP p) -> Decl p
pattern InfixD' a p n = InfixD () a p n

type Decl' p = XRec p Decl

data Assoc = InfixL
           | InfixR
           | Infix
           deriving (Show)

data ConAlt p = ConAlt (IdP p) [Type]

data RlpExpr p = LetE  (XLetE p)   [Bind p] (RlpExpr' p)
               | VarE  (XVarE p)   (IdP p)
               | LamE  (XLamE p)   [Pat p] (RlpExpr' p)
               | CaseE (XCaseE p)  (RlpExpr' p) [(Alt p, Where p)]
               | IfE   (XIfE p)    (RlpExpr' p) (RlpExpr' p) (RlpExpr' p)
               | AppE  (XAppE p)   (RlpExpr' p) (RlpExpr' p)
               | LitE  (XLitE p)   (Lit p)
               | ParE  (XParE p)   (RlpExpr' p)
               | OAppE (XOAppE p)  (IdP p) (RlpExpr' p) (RlpExpr' p)
               | XRlpExpr !(XXRlpExpr p)

type RlpExpr' p = XRec p RlpExpr

class UnXRec p where
    unXRec :: XRec p f -> f p

class MapXRec p where
    mapXRec :: (f p -> f p) -> XRec p f -> XRec p f

type family XRec p (f :: * -> *) = (r :: *) | r -> p f

type family XLetE p
type family XVarE p
type family XConE p
type family XLamE p
type family XCaseE p
type family XIfE p
type family XAppE p
type family XLitE p
type family XParE p
type family XOAppE p
type family XXRlpExpr p

type family IdP p

pattern ParE' :: (XParE p ~ ()) => RlpExpr' p -> RlpExpr p
pattern ParE' e = ParE () e

pattern LitE' :: (XLitE p ~ ()) => Lit p -> RlpExpr p
pattern LitE' e = LitE () e

pattern VarE' :: (XVarE p ~ ()) => IdP p -> RlpExpr p
pattern VarE' e = VarE () e

type Where p = [Bind p]

-- do we want guards?
data Alt p = AltA (Pat' p) (RlpExpr' p)

data Bind p = PatB (Pat' p) (RlpExpr' p)
            | FunB (IdP p) [Pat' p] (RlpExpr' p)

data Pat p = VarP (IdP p)
           | LitP (Lit' p)
           | ConP (IdP p) [Pat' p]

type Pat' p = XRec p Pat

data Lit p = IntL Int
           | CharL Char
           | ListL [RlpExpr' p]

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

