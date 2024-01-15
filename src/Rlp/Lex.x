{
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.Lex
    ( P(..)
    , RlpToken(..)
    , Located(..)
    , lexer
    , lexerCont
    )
    where
import Control.Monad
import Data.Functor.Identity
import Data.Char                (digitToInt)
import Core.Syntax              (Name)
import Data.Monoid              (First)
import Data.Maybe
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Word
import Data.Default
import Lens.Micro.Mtl
import Lens.Micro
import Lens.Micro.TH

import Debug.Trace
}

$whitechar      = [ \t\n\r\f\v]

$lower          = [a-z \_]
$upper          = [A-Z]
$alpha          = [$lower $upper]
$digit          = 0-9

$nl             = [\n\r]
$white_no_nl    = $white # $nl

$namechar       = [$alpha $digit \' \#]

@varname        = $lower $namechar*

@digits         = $digit+

rlp :-
    
    -- skip whitespace
    $white_no_nl+       ;
    -- TODO: don't treat operators like (-->) as comments
    "--".*              ;
    ";"                 { constToken TokenSemicolon }
    -- "{"                 { explicitLBrace }
    -- "}"                 { explicitRBrace }

<0>
{
    \n                  { begin bol }
}

<one>
{
    @varname            { tokenWith TokenVarName }
    @digits             { tokenWith (TokenLitInt . readInt) }
    "="                 { constToken TokenEquals }
    \n                  { begin bol }
}

-- consume all whitespace leaving us at the beginning of the next non-empty
-- line. we then compare the indentation of that line to the enclosing layout
-- context and proceed accordingly
<bol>
{
    $whitechar          ;
    \n                  ;
    ()                  { doBol }
}

{

begin = undefined

type LexerAction a = AlexInput -> Int -> P a

type AlexInput =
    ( Char      -- prev char
    , Text      -- input
    )

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte (_,s) = undefined

getInput :: P AlexInput
getInput = undefined

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = (^. _1)

readInt :: Text -> Int
readInt = T.foldr f 0 where
    f c n = digitToInt c + 10*n

-- | @andBegin@, with the subtle difference that the start code is set
--   /after/ the action
thenBegin :: LexerAction a -> Int -> LexerAction a
thenBegin act c inp l = do
    a <- act inp l
    undefined

constToken :: RlpToken -> LexerAction (Located RlpToken)
constToken t inp _ = undefined

tokenWith :: (Text -> RlpToken) -> LexerAction (Located RlpToken)
tokenWith tf inp l = undefined

alexEOF :: P (Located RlpToken)
alexEOF = do
    inp <- getInput
    pure (Located undefined TokenEOF)

data RlpToken
    -- literals
    = TokenLitInt Int
    -- identifiers
    | TokenVarName Name
    | TokenConName Name
    | TokenVarSym Name
    | TokenConSym Name
    -- keywords
    | TokenData
    | TokenPipe
    | TokenLet
    | TokenIn
    -- control symbols
    | TokenEquals
    | TokenSemicolon
    | TokenLBrace
    | TokenRBrace
    -- 'virtual' control symbols, inserted by the lexer without any correlation
    -- to a specific symbol
    | TokenSemicolonV
    | TokenLBraceV
    | TokenRBraceV
    | TokenEOF
    deriving (Show)

newtype P a = P { runP :: ParseState -> (ParseState, Maybe a) }
    deriving (Functor)

execP :: P a -> ParseState -> Either String a
execP p st = undefined

execP' :: P a -> Text -> Either String a
execP' p s = execP p st where
    st = initParseState s

initParseState :: Text -> ParseState
initParseState s = ParseState
    { _psLayoutStack = []
    , _psLexState = [bol,0]
    , _psInput = (undefined, s)
    }

data ParseState = ParseState
    { _psLayoutStack        :: [Layout]
    , _psLexState           :: [Int]
    , _psInput              :: AlexInput
    }

instance Applicative P where
    pure a = P $ \st -> (st,Just a)
    liftA2 = liftM2

instance Monad P where
    p >>= k = undefined

data Layout = Explicit
            | Implicit Int
            deriving (Show, Eq)

data Located a = Located (Int, Int) a
    deriving (Show)

lexer :: P (Located RlpToken)
lexer = undefined

lexerCont :: (Located RlpToken -> P a) -> P a
lexerCont = undefined

lexStream :: P [RlpToken]
lexStream = undefined

lexTest :: Text -> Either String [RlpToken]
lexTest = undefined

lexToken :: P (Located RlpToken)
lexToken = undefined

doBol = undefined

}

