{-# LANGUAGE ImplicitParams, ViewPatterns, PatternSynonyms #-}
{-
Description : Supporting types for the parser
-}
module Rlp.Parse.Types
    (
    -- * Partial ASTs
      Partial(..)
    , PartialE
    , PartialExpr'
    , PartialDecl'
    , pattern WithInfo
    , pR
    , pL

    -- * Parser types
    , Parser
    , ParserState(..)
    , OpTable
    , OpInfo
    )
    where
----------------------------------------------------------------------------------
import Control.Monad.State
import Data.HashMap.Strict          qualified as H
import Data.Fix
import Data.Functor.Foldable
import Data.Functor.Const
import Data.Functor.Classes
import Data.Void
import Data.Maybe
import Text.Megaparsec              hiding (State)
import Lens.Micro
import Rlp.Syntax
----------------------------------------------------------------------------------

-- parser types

type Parser = ParsecT Void Text (State ParserState)

data ParserState = ParserState
    { _psOpTable :: OpTable
    }
    deriving Show

type OpTable = H.HashMap Name OpInfo
type OpInfo = (Assoc, Int)

----------------------------------------------------------------------------------

-- absolute psycho shit (partial ASTs)

type PartialDecl' = Decl (Const PartialExpr') Name

data Partial a = E (RlpExprF Name a)
               | B Name (Partial a) (Partial a)
               | P (Partial a)
               deriving (Show, Functor)

pL :: Traversal' (Partial a) (Partial a)
pL k (B o l r) = (\l' -> B o l' r) <$> k l
pL _ x         = pure x

pR :: Traversal' (Partial a) (Partial a)
pR k (B o l r) = (\r' -> B o l r') <$> k r
pR _ x         = pure x

type PartialE = Partial RlpExpr'

-- i love you haskell
pattern WithInfo :: (?pt :: OpTable) => OpInfo -> PartialE -> PartialE -> PartialE
pattern WithInfo p l r <- B (opInfoOrDef -> p) l r

opInfoOrDef :: (?pt :: OpTable) => Name -> OpInfo
opInfoOrDef c = fromMaybe (InfixL,9) $ H.lookup c ?pt

-- required to satisfy constraint on Fix's show instance
instance Show1 Partial where
    liftShowsPrec :: forall a. (Int -> a -> ShowS)
                  -> ([a] -> ShowS)
                  -> Int -> Partial a -> ShowS

    liftShowsPrec sp sl p m = case m of
        (E e)       -> showsUnaryWith lshow "E" p e
        (B f a b)   -> showsTernaryWith showsPrec lshow lshow "B" p f a b
        (P e)       -> showsUnaryWith lshow "P" p e
        where
            lshow :: forall f. (Show1 f) => Int -> f a -> ShowS
            lshow = liftShowsPrec sp sl

type PartialExpr' = Fix Partial

