{
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.Lex
    ( P(..)
    , RlpToken(..)
    , Located(..)
    , AlexPosn
    , lexer
    , lexerCont
    )
    where
import Control.Monad
import Data.Functor.Identity
import Core.Syntax              (Name)
import Data.Monoid              (First)
import Data.Maybe
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Default
import Lens.Micro.Mtl
import Lens.Micro
import Lens.Micro.TH

import Debug.Trace
}

%wrapper "monadUserState-strict-text"

$whitechar      = [ \t\n\r\f\v]

$lower          = [a-z \_]
$upper          = [A-Z]
$alpha          = [$lower $upper]
$digit          = 0-9

$nl             = [\n\r]
$white_no_nl    = $white # $nl

$namechar       = [$alpha $digit \' \#]

@varname        = $lower $namechar*

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
    \n                  ;
    "{"                 { explicitLBrace `thenBegin` one }
    ()                  { doLayout `thenBegin` one }
}

<one>
{
    @varname            { tokenWith TokenVarName }
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
    ()                  { doBol `andBegin` one }
}

{

-- | @andBegin@, with the subtle difference that the start code is set
--   /after/ the action
thenBegin :: AlexAction a -> Int -> AlexAction a
thenBegin act c inp l = do
    a <- act inp l
    alexSetStartCode c
    pure a

constToken :: RlpToken -> AlexAction (Located RlpToken)
constToken t inp _ = pure $ Located (inp ^. _1) t

tokenWith :: (Text -> RlpToken) -> AlexAction (Located RlpToken)
tokenWith tf (p,_,_,s) l = pure $ Located p (tf $ T.take l s)

alexEOF :: Alex (Located RlpToken)
alexEOF = do
    inp <- alexGetInput
    pure (Located (inp ^. _1) TokenEOF)

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

newtype P a = P { runP :: ParseState -> Alex (ParseState, a) }
    deriving (Functor)

execP :: P a -> ParseState -> Text -> Either String a
execP p st s = snd <$> runAlex s (runP p st)

data ParseState = ParseState { }

instance Default ParseState where
    def = ParseState { }

instance Applicative P where
    pure a = P $ \st -> pure (st,a)
    liftA2 = liftM2

instance Monad P where
    p >>= k = P $ \st -> do
        (st',a) <- runP p st
        runP (k a) st'

data AlexUserState = AlexUserState
    -- the layout context, along with a start code to return to when the layout
    -- ends
    { _ausLayoutStack     :: [(Layout, Int)]
    }
    deriving Show

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { _ausLayoutStack = []
    }

data Layout = Explicit
            | Implicit Int
            deriving (Show, Eq)

data Located a = Located AlexPosn a
    deriving (Show)

ausLayoutStack :: Lens' AlexUserState [(Layout, Int)]
ausLayoutStack = lens _ausLayoutStack
    (\ s l -> s { _ausLayoutStack = l })

lexer :: P (Located RlpToken)
lexer = P $ \st -> (st,) <$> lexToken

lexerCont :: (Located RlpToken -> P a) -> P a
lexerCont = (lexer >>=)

lexStream :: Alex [RlpToken]
lexStream = do
    t <- lexToken
    case t of
        Located _ TokenEOF  -> pure [TokenEOF]
        Located _ a         -> (a:) <$> lexStream

lexTest :: Text -> Either String [RlpToken]
lexTest = flip runAlex lexStream

lexToken :: Alex (Located RlpToken)
lexToken = alexMonadScan

getsAus :: (AlexUserState -> b) -> Alex b
getsAus k = alexGetUserState <&> k

useAus :: Getting a AlexUserState a -> Alex a
useAus l = do
    aus <- alexGetUserState
    pure (aus ^. l)

preuseAus :: Getting (First a) AlexUserState a -> Alex (Maybe a)
preuseAus l = do
    aus <- alexGetUserState
    pure (aus ^? l)

modifyingAus :: ASetter' AlexUserState a -> (a -> a) -> Alex () 
modifyingAus l f = do
    aus <- alexGetUserState
    alexSetUserState (aus & l %~ f)

indentLevel :: Alex Int
indentLevel = do
    inp <- alexGetInput
    let col = inp ^. _1
            & \ (AlexPn _ _ c) -> c
    pure col

cmpLayout :: Alex Ordering
cmpLayout = do
    i <- indentLevel
    ctx <- preuseAus (ausLayoutStack . _head)
    case (ctx <&> fst) ^. non (Implicit 1) of
        Implicit n -> pure (i `compare` n)
        Explicit   -> pure GT

insertToken :: RlpToken -> Alex (Located RlpToken)
insertToken t = do
    inp <- alexGetInput
    pure (Located (inp ^. _1) t)

insertSemicolon, insertLBrace, insertRBrace :: Alex (Located RlpToken)
insertSemicolon = traceM "inserting semi" >> insertToken TokenSemicolonV
insertLBrace = traceM "inserting lbrace" >> insertToken TokenLBraceV
insertRBrace = traceM "inserting rbrace" >> insertToken TokenRBraceV

-- pop the layout stack and jump to the popped return code
popLayout :: Alex ()
popLayout = do
    traceM "pop layout"
    ctx <- preuseAus (ausLayoutStack . _head)
    modifyingAus ausLayoutStack (drop 1)
    case ctx of
        Just (l,c)  -> alexSetStartCode c
        Nothing     -> pure ()

pushLayout :: Layout -> Alex ()
pushLayout l = do
    traceM "push layout"
    c <- alexGetStartCode
    modifyingAus ausLayoutStack ((l,c):)

doBol :: AlexAction (Located RlpToken)
doBol inp len = do
    off <- cmpLayout
    case off of
        -- the line is aligned with the previous. it therefore belongs to the
        -- same list
        EQ -> insertSemicolon
        -- the line is indented further than the previous, so we assume it is a
        -- line continuation. ignore it and move on!
        GT -> undefined -- alexSetStartCode one >> lexToken
        -- the line is indented less than the previous, pop the layout stack and
        -- insert a closing brace.
        LT -> insertRBrace >> popLayout >> lexToken

explicitLBrace, explicitRBrace :: AlexAction (Located RlpToken)

explicitLBrace _ _ = do
    pushLayout Explicit
    insertToken TokenLBrace

explicitRBrace _ _ = do
    popLayout
    insertToken TokenRBrace

doLayout :: AlexAction (Located RlpToken)
doLayout _ _ = do
    i <- indentLevel
    pushLayout (Implicit i)
    insertLBrace

}

