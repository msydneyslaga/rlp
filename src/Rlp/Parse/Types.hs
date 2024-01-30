{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Rlp.Parse.Types
    (
    -- * Trees That Grow
      RlpcPs

    -- * Parser monad and state
    , P(..), ParseState(..), Layout(..), OpTable, OpInfo
    -- ** Lenses
    , psLayoutStack, psLexState, psInput, psOpTable

    -- * Other parser types
    , RlpToken(..), AlexInput(..), Position(..), spanFromPos, LexerAction
    , Located(..), PsName
    -- ** Lenses
    , aiPrevChar, aiSource, aiBytes, aiPos, posLine, posColumn

    , (<<~), (<~>)

    -- * Error handling
    , MsgEnvelope(..), RlpcError(..), RlpParseError(..)
    , addFatal, addWound, addFatalHere, addWoundHere
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
import Data.Void
import Data.Word                    (Word8)
import Lens.Micro.TH
import Lens.Micro
import Rlp.Syntax
import Compiler.Types
--------------------------------------------------------------------------------

-- | Phantom type identifying rlpc's parser phase

data RlpcPs

type instance XRec RlpcPs f = Located (f RlpcPs)
type instance IdP RlpcPs = PsName

type instance XFunD RlpcPs = ()
type instance XDataD RlpcPs = ()
type instance XInfixD RlpcPs = ()
type instance XTySigD RlpcPs = ()
type instance XXDeclD RlpcPs = ()

type instance XLetE RlpcPs = ()
type instance XVarE RlpcPs = ()
type instance XLamE RlpcPs = ()
type instance XCaseE RlpcPs = ()
type instance XIfE RlpcPs = ()
type instance XAppE RlpcPs = ()
type instance XLitE RlpcPs = ()
type instance XParE RlpcPs = ()
type instance XOAppE RlpcPs = ()

type PsName = Text

--------------------------------------------------------------------------------

spanFromPos :: Position -> Int -> SrcSpan
spanFromPos (l,c,a) s = SrcSpan l c a s

{-# INLINE spanFromPos #-}

type LexerAction a = AlexInput -> Int -> P a

data AlexInput = AlexInput
    { _aiPrevChar   :: Char
    , _aiSource     :: Text
    , _aiBytes      :: [Word8]
    , _aiPos        :: Position
    }
    deriving Show

type Position =
    ( Int -- ^ line
    , Int -- ^ column
    , Int -- ^ Absolutely
    )

posLine :: Lens' Position Int
posLine = _1

posColumn :: Lens' Position Int
posColumn = _2

posAbsolute :: Lens' Position Int
posAbsolute = _3

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
    -- to a specific part of the input
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

makeLenses ''AlexInput
makeLenses ''ParseState

addWoundHere :: Int -> RlpParseError -> P ()
addWoundHere l e = P $ \st ->
    let e' = MsgEnvelope
            { _msgSpan = let pos = psInput . aiPos
                         in SrcSpan (st ^. pos . posLine)
                                    (st ^. pos . posColumn)
                                    (st ^. pos . posAbsolute)
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
                                    (st ^. pos . posAbsolute)
                                    l
            , _msgDiagnostic = e
            , _msgSeverity = SevError
            }
    in (st, [e'], Nothing)

