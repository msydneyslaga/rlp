-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- recursion-schemes
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.Syntax
    ( RlpExpr(..)
    , RlpExprF(..)
    , RlpExprF'
    , Decl(..)
    , Assoc(..)
    , VarId(..)
    , Pat(..)
    , Pat'
    )
    where
----------------------------------------------------------------------------------
import Data.Functor.Const
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Lens.Micro
import Core.Syntax                  hiding (Lit)
import Core                         (HasRHS(..), HasLHS(..))
----------------------------------------------------------------------------------

newtype RlpProgram b = RlpProgram [Decl RlpExpr b]

-- | The @e@ parameter is used for partial results. When parsing an input, we
-- first parse all top-level declarations in order to extract infix[lr]
-- declarations. This process yields a @[Decl (Const Text) Name]@, where @Const
-- Text@ stores the remaining unparsed function bodies. Once infixities are
-- accounted for, we may complete the parsing task and get a proper @[Decl
-- RlpExpr Name]@.

data Decl e b = FunD    VarId [Pat b] (e b)
              | TySigD  [VarId] Type
              | DataD   ConId [ConId] [ConAlt]
              | InfixD  Assoc Int Name
              deriving Show

data Assoc = InfixL
           | InfixR
           | Infix
           deriving Show

data ConAlt = ConAlt ConId [ConId]
               deriving Show

data RlpExpr b = LetE  [Bind b] (RlpExpr b)
               | VarE  VarId
               | ConE  ConId
               | LamE  [Pat b] (RlpExpr b)
               | CaseE (RlpExpr b) [Alt b]
               | IfE   (RlpExpr b) (RlpExpr b) (RlpExpr b)
               | AppE  (RlpExpr b) (RlpExpr b)
               | LitE  (Lit b)
               deriving Show

-- do we want guards?
data Alt b = AltA (Pat b) (RlpExpr b)
               deriving Show

data Bind b = PatB (Pat b) (RlpExpr b)
            | FunB VarId [Pat b] (RlpExpr b)
            deriving Show

data VarId = NameVar Text
           | SymVar Text
           deriving Show

instance IsString VarId where
    -- TODO: use symvar if it's an operator
    fromString = NameVar . T.pack

data ConId = NameCon Text
           | SymCon Text
           deriving Show

data Pat b = VarP VarId
           | LitP (Lit b)
           | ConP ConId [Pat b]
           deriving Show

type Pat' = Pat Name

data Lit b = IntL Int
           | CharL Char
           | ListL [RlpExpr b]
           deriving Show

-- instance HasLHS Alt Alt Pat Pat where
--     _lhs = lens
--         (\ (AltA p _) -> p)
--         (\ (AltA _ e) p' -> AltA p' e)

-- instance HasRHS Alt Alt RlpExpr RlpExpr where
--     _rhs = lens
--         (\ (AltA _ e) -> e)
--         (\ (AltA p _) e' -> AltA p e')

makeBaseFunctor ''RlpExpr

deriving instance (Show b, Show a) => Show (RlpExprF b a)

type RlpExprF' = RlpExprF Name

