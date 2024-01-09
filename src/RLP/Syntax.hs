-- recursion-schemes
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- recursion-schemes
{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module Rlp.Syntax
    ( RlpExpr(..)
    , RlpExpr'
    , RlpExprF(..)
    , RlpExprF'
    , Decl(..)
    , Decl'
    , Bind(..)
    , Where
    , Where'
    , ConAlt(..)
    , Type(..)
    , pattern (:->)
    , Assoc(..)
    , VarId(..)
    , ConId(..)
    , Pat(..)
    , Pat'
    , Lit(..)
    , Lit'
    , Name

    -- TODO: ugh move this somewhere else later
    , showsTernaryWith

    -- * Convenience re-exports
    , Text
    )
    where
----------------------------------------------------------------------------------
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.String                  (IsString(..))
import Data.Functor.Foldable.TH     (makeBaseFunctor)
import Data.Functor.Classes
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

data Decl e b = FunD    VarId [Pat b] (e b) (Where b)
              | TySigD  [VarId] Type
              | DataD   ConId [Name] [ConAlt]
              | InfixD  Assoc Int Name
              deriving Show

type Decl' e = Decl e Name

data Assoc = InfixL
           | InfixR
           | Infix
           deriving Show

data ConAlt = ConAlt ConId [Type]
            deriving Show

data RlpExpr b = LetE  [Bind b] (RlpExpr b)
               | VarE  VarId
               | ConE  ConId
               | LamE  [Pat b] (RlpExpr b)
               | CaseE (RlpExpr b) [(Alt b, Where b)]
               | IfE   (RlpExpr b) (RlpExpr b) (RlpExpr b)
               | AppE  (RlpExpr b) (RlpExpr b)
               | LitE  (Lit b)
               deriving Show

type RlpExpr' = RlpExpr Name

type Where b = [Bind b]
type Where' = [Bind Name]

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

type Lit' = Lit Name

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

-- society if derivable Show1
instance (Show b) => Show1 (RlpExprF b) where
    liftShowsPrec sp _ p m = case m of
        (LetEF bs e)            -> showsBinaryWith showsPrec sp "LetEF" p bs e
        (VarEF n)               -> showsUnaryWith showsPrec "VarEF" p n
        (ConEF n)               -> showsUnaryWith showsPrec "ConEF" p n
        (LamEF bs e)            -> showsBinaryWith showsPrec sp "LamEF" p bs e
        (CaseEF e as)           -> showsBinaryWith sp showsPrec "CaseEF" p e as
        (IfEF a b c)            -> showsTernaryWith sp sp sp "IfEF" p a b c
        (AppEF f x)             -> showsBinaryWith sp sp "AppEF" p f x
        (LitEF l)               -> showsUnaryWith showsPrec "LitEF" p l

showsTernaryWith :: (Int -> x -> ShowS)
                 -> (Int -> y -> ShowS)
                 -> (Int -> z -> ShowS)
                 -> String -> Int
                 -> x -> y -> z
                 -> ShowS
showsTernaryWith sa sb sc name p a b c = showParen (p > 10)
    $ showString name
    . showChar ' ' . sa 11 a 
    . showChar ' ' . sb 11 b
    . showChar ' ' . sc 11 c

