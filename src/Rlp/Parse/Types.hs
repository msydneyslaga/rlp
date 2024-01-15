{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Rlp.Parse.Types where
--------------------------------------------------------------------------------
import Core.Syntax                  (Name)
import Control.Monad
import Control.Monad.State.Class
import Data.Text                    (Text)
import Data.Maybe
import Data.Fix
import Data.Functor.Foldable
import Data.Functor.Const
import Data.Functor.Classes
import Data.HashMap.Strict          qualified as H
import Data.Word                    (Word8)
import Lens.Micro.TH
import Lens.Micro
import Rlp.Syntax
--------------------------------------------------------------------------------

type LexerAction a = AlexInput -> Int -> P a

data AlexInput = AlexInput
    { _aiPrevChar   :: Char
    , _aiSource     :: Text
    , _aiBytes      :: [Word8]
    , _aiPos        :: Position
    }
    deriving Show

type Position =
    ( Int -- line
    , Int -- column
    )

data RlpToken
    -- literals
    = TokenLitInt Int
    -- identifiers
    | TokenVarName Name
    | TokenConName Name
    | TokenVarSym Name
    | TokenConSym Name
    -- reserved words
    | TokenData
    | TokenCase
    | TokenOf
    | TokenLet
    | TokenIn
    -- reserved ops
    | TokenArrow
    | TokenPipe
    | TokenHasType
    | TokenLambda
    | TokenEquals
    -- control symbols
    | TokenSemicolon
    | TokenLBrace
    | TokenRBrace
    | TokenLParen
    | TokenRParen
    -- 'virtual' control symbols, inserted by the lexer without any correlation
    -- to a specific symbol
    | TokenSemicolonV
    | TokenLBraceV
    | TokenRBraceV
    | TokenEOF
    deriving (Show)

newtype P a = P { runP :: ParseState -> (ParseState, Maybe a) }
    deriving (Functor)

instance Applicative P where
    pure a = P $ \st -> (st,Just a)
    liftA2 = liftM2

instance Monad P where
    p >>= k = P $ \st ->
        let (st',a) = runP p st
        in case a of
            Just x  -> runP (k x) st'
            Nothing -> (st', Nothing)

instance MonadState ParseState P where
    state f = P $ \st ->
        let (a,st') = f st
        in (st', Just a)

data ParseState = ParseState
    { _psLayoutStack        :: [Layout]
    , _psLexState           :: [Int]
    , _psInput              :: AlexInput
    , _psOpTable            :: OpTable
    }
    deriving Show

data Layout = Explicit
            | Implicit Int
            deriving (Show, Eq)

data Located a = Located (Position, Int) a
    deriving (Show)

type OpTable = H.HashMap Name OpInfo
type OpInfo = (Assoc, Int)

-- data WithLocation a = WithLocation [String] a

data RlpParseError = RlpParErrOutOfBoundsPrecedence Int
                   | RlpParErrDuplicateInfixD
    deriving (Eq, Ord, Show)

----------------------------------------------------------------------------------

-- absolute psycho shit (partial ASTs)

type PartialDecl' = Decl (Const PartialExpr') Name

data Partial a = E (RlpExprF Name a)
               | B Name (Partial a) (Partial a)
               | Par (Partial a)
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
        (Par e)     -> showsUnaryWith lshow "Par" p e
        where
            lshow :: forall f. (Show1 f) => Int -> f a -> ShowS
            lshow = liftShowsPrec sp sl

type PartialExpr' = Fix Partial

makeLenses ''AlexInput
makeLenses ''ParseState

