{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Rlp.Parse.Types
    ( LexerAction
    , MsgEnvelope(..)
    , RlpcError(..)
    , AlexInput(..)
    , Position(..)
    , RlpToken(..)
    , P(..)
    , ParseState(..)
    , psLayoutStack
    , psLexState
    , psInput
    , psOpTable
    , Layout(..)
    , Located(..)
    , OpTable
    , OpInfo
    , RlpParseError(..)
    , PartialDecl'
    , Partial(..)
    , pL, pR
    , PartialE
    , pattern WithInfo
    , opInfoOrDef
    , PartialExpr'
    , aiPrevChar
    , aiSource
    , aiBytes
    , aiPos
    , addFatal
    , addWound
    , addFatalHere
    , addWoundHere
    )
    where
--------------------------------------------------------------------------------
import Core.Syntax                  (Name)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Errorful
import Compiler.RlpcError
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

posLine :: Lens' Position Int
posLine = _1

posColumn :: Lens' Position Int
posColumn = _2

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
    | TokenInfixL
    | TokenInfixR
    | TokenInfix
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

newtype P a = P {
        runP :: ParseState
             -> (ParseState, [MsgEnvelope RlpParseError], Maybe a)
    }
    deriving (Functor)

instance Applicative P where
    pure a = P $ \st -> (st, [], pure a)
    liftA2 = liftM2

instance Monad P where
    p >>= k = P $ \st ->
        let (st',es,ma) = runP p st
        in case ma of
            Just a  -> runP (k a) st'
                     & _2 %~ (es<>)
            Nothing -> (st',es,Nothing)

    {-# INLINE (>>=) #-}

instance MonadState ParseState P where
    state f = P $ \st ->
        let (a,st') = f st
        in (st', [], Just a)

instance MonadErrorful (MsgEnvelope RlpParseError) P where
    addWound e = P $ \st -> (st, [e], Just ())
    addFatal e = P $ \st -> (st, [e], Nothing)

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
                   | RlpParErrDuplicateInfixD Name
                   | RlpParErrLexical
                   | RlpParErrUnexpectedToken
                   deriving (Eq, Ord, Show)

instance IsRlpcError RlpParseError where

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

addWoundHere :: Int -> RlpParseError -> P ()
addWoundHere l e = P $ \st ->
    let e' = MsgEnvelope
            { _msgSpan = let pos = psInput . aiPos
                         in SrcSpan (st ^. pos . posLine)
                                   (st ^. pos . posColumn)
                                   l
            , _msgDiagnostic = e
            , _msgSeverity = SevError
            }
    in (st, [e'], Just ())

addFatalHere :: Int -> RlpParseError -> P a
addFatalHere l e = P $ \st ->
    let e' = MsgEnvelope
            { _msgSpan = let pos = psInput . aiPos
                         in SrcSpan (st ^. pos . posLine)
                                    (st ^. pos . posColumn)
                                    l
            , _msgDiagnostic = e
            , _msgSeverity = SevError
            }
    in (st, [e'], Nothing)

