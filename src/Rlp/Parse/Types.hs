{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ImplicitParams, ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Rlp.Parse.Types
    (
    -- * Trees That Grow
      RlpcPs

    -- * Parser monad and state
    , P(..), ParseState(..), Layout(..), OpTable, OpInfo
    , initParseState, initAlexInput
    , pToErrorful
    -- ** Lenses
    , psLayoutStack, psLexState, psInput, psOpTable

    -- * Other parser types
    , RlpToken(..), AlexInput(..), Position(..), spanFromPos, LexerAction
    , Located(..), PsName
    , srcSpanLen
    -- ** Lenses
    , _TokenLitInt, _TokenVarName, _TokenConName, _TokenVarSym, _TokenConSym
    , aiPrevChar, aiSource, aiBytes, aiPos, posLine, posColumn

    -- * Error handling
    , MsgEnvelope(..), RlpcError(..), RlpParseError(..)
    , addFatal, addWound, addFatalHere, addWoundHere
    )
    where
--------------------------------------------------------------------------------
import Core.Syntax                  (Name)
import Text.Show.Deriving
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Errorful
import Control.Comonad              (extract)
import Compiler.RlpcError
import Language.Haskell.TH.Syntax   (Lift)
import Data.Text                    (Text)
import Data.Maybe
import Data.Fix
import Data.Functor.Foldable
import Data.Functor.Const
import Data.Functor.Classes
import Data.HashMap.Strict          qualified as H
import Data.Void
import Data.Word                    (Word8)
import Data.Text                    qualified as T
import Control.Lens                 hiding ((<<~))
import Rlp.Syntax
import Compiler.Types
--------------------------------------------------------------------------------

-- | Phantom type identifying rlpc's parser phase

data RlpcPs

type instance NameP RlpcPs = PsName

type PsName = Located Text

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
    | TokenVarName Text
    | TokenConName Text
    | TokenVarSym Text
    | TokenConSym Text
    -- reserved words
    | TokenData
    | TokenCase
    | TokenOf
    | TokenLet
    | TokenLetrec
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

_TokenLitInt :: Prism' RlpToken Int
_TokenLitInt = prism TokenLitInt $ \case
    TokenLitInt n -> Right n
    x             -> Left x

_TokenVarName :: Prism' RlpToken Text
_TokenVarName = prism TokenVarName $ \case
    TokenVarName n -> Right n
    x              -> Left x

_TokenVarSym :: Prism' RlpToken Text
_TokenVarSym = prism TokenVarSym $ \case
    TokenVarSym n -> Right n
    x              -> Left x

_TokenConName :: Prism' RlpToken Text
_TokenConName = prism TokenConName $ \case
    TokenConName n -> Right n
    x              -> Left x

_TokenConSym :: Prism' RlpToken Text
_TokenConSym = prism TokenConSym $ \case
    TokenConSym n -> Right n
    x              -> Left x

newtype P a = P {
        runP :: ParseState
             -> (ParseState, [MsgEnvelope RlpParseError], Maybe a)
    }
    deriving (Functor)

pToErrorful :: (Applicative m)
            => P a -> ParseState -> ErrorfulT (MsgEnvelope RlpParseError) m a
pToErrorful p st = ErrorfulT $ pure (ma,es) where
    (_,es,ma) = runP p st

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

data RlpParseError = RlpParErrOutOfBoundsPrecedence Int
                   | RlpParErrDuplicateInfixD Name
                   | RlpParErrLexical
                   | RlpParErrUnexpectedToken RlpToken [String]
                   | RlpParErrOther [Text]
                   deriving (Show)

instance IsRlpcError RlpParseError where
    liftRlpcError = \case
        RlpParErrOutOfBoundsPrecedence n ->
            Text [ "Illegal precedence in infixity declaration"
                 , "rl' currently only allows precedences between 0 and 9."
                 ]
        RlpParErrDuplicateInfixD s ->
            Text [ "Conflicting infixity declarations for operator "
                    <> tshow s
                 ]
        RlpParErrLexical ->
            Text [ "Unknown lexical error :(" ]
        RlpParErrUnexpectedToken t exp ->
            Text [ "Unexpected token " <> tshow t
                 , "Expected: " <> tshow exp
                 ]
        RlpParErrOther ts ->
            Text ts
        where
            tshow :: (Show a) => a -> T.Text
            tshow = T.pack . show

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

initParseState :: [Int] -> Text -> ParseState
initParseState ls s = ParseState
    { _psLayoutStack = []
    -- IMPORTANT: the initial state is `bol` to begin the top-level layout,
    -- which then returns to state 0 which continues the normal lexing process.
    , _psLexState = ls
    , _psInput = initAlexInput s
    , _psOpTable = mempty
    }

initAlexInput :: Text -> AlexInput
initAlexInput s = AlexInput
    { _aiPrevChar   = '\0'
    , _aiSource     = s
    , _aiBytes      = []
    , _aiPos        = (1,1,0)
    }

--------------------------------------------------------------------------------


-- deriving instance Lift (Program RlpcPs)
-- deriving instance Lift (Decl RlpcPs)
-- deriving instance Lift (Pat RlpcPs)
-- deriving instance Lift (Lit RlpcPs)
-- deriving instance Lift (Expr RlpcPs)
-- deriving instance Lift (Binding RlpcPs)
-- deriving instance Lift (Ty RlpcPs)
-- deriving instance Lift (Alt RlpcPs)
-- deriving instance Lift (ConAlt RlpcPs)

