{
{-# LANGUAGE ViewPatterns, LambdaCase #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.Lex
    ( P(..)
    , RlpToken(..)
    , Located(..)
    , lexToken
    , lexStream
    , lexDebug
    , lexCont
    , popLexState
    , programInitState
    , runP'
    )
    where
import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad
import Control.Monad.Errorful
import Core.Syntax              (Name)
import Data.Functor.Identity
import Data.Char                (digitToInt)
import Data.Monoid              (First)
import Data.Maybe
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Word
import Data.Default
import Lens.Micro.Mtl
import Lens.Micro

import Debug.Trace
import Rlp.Parse.Types
}

$whitechar      = [ \t\n\r\f\v]

$nl             = [\n\r]
$white_no_nl    = $white # $nl

$lower          = [a-z \_]
$upper          = [A-Z]
$alpha          = [$lower $upper]
$digit          = 0-9

$special        = [\(\)\,\;\[\]\{\}]
$namechar       = [$alpha $digit \' \#]
$asciisym       = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

@decimal        = $digit+

@varname        = $lower $namechar*
@conname        = $upper $namechar*
@consym         = \: $asciisym*
@varsym         = $asciisym+

@reservedname = 
    case|data|do|import|in|let|letrec|module|of|where
    |infixr|infixl|infix

@reservedop =
    "=" | \\ | "->" | "|" | "::"

rlp :-
    
-- everywhere: skip whitespace
$white_no_nl+       ;

-- everywhere: skip comments
-- TODO: don't treat operators like (-->) as comments
"--".*              ;

-- we are indentation-sensitive! do not skip NLs!. upon encountering a newline,
-- we check indentation and potentially insert extra tokens. search this file
-- for the definition of `doBol`
<0> \n                  { beginPush bol }

<layout>
{
    
}

-- layout keywords
<0>
{
    "let"               { constToken TokenLet `thenBeginPush` layout_let }
}

-- scan various identifiers and reserved words. order is important here!
<0>
{
    @reservedname       { tokenWith lexReservedName }
    @conname            { tokenWith TokenConName }
    @varname            { tokenWith TokenVarName }
    @reservedop         { tokenWith lexReservedOp }
    @consym             { tokenWith TokenConSym }
    @varsym             { tokenWith TokenVarSym }
}

-- literals -- currently this is just unsigned integer literals
<0>
{
    @decimal            { tokenWith (TokenLitInt . readInt) }
}

-- control characters
<0>
{
    "("                 { constToken TokenLParen }
    ")"                 { constToken TokenRParen }
    "{"                 { explicitLBrace }
    "}"                 { explicitRBrace }
    ";"                 { constToken TokenSemicolon }
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

<layout_let>
{
    \n                  { beginPush bol }
    "{"                 { explicitLBrace }
    "in"                { constToken TokenIn `thenDo` (popLexState *> popLayout) }
    ()                  { doLayout }
}

<layout_top>
{
    \n                  ;
    "{"                 { explicitLBrace `thenDo` popLexState }
    ()                  { doLayout }
}

{

lexReservedName :: Text -> RlpToken
lexReservedName = \case
    "data"      -> TokenData
    "case"      -> TokenCase
    "of"        -> TokenOf
    "let"       -> TokenLet
    "in"        -> TokenIn
    "infix"     -> TokenInfix
    "infixl"    -> TokenInfixL
    "infixr"    -> TokenInfixR

lexReservedOp :: Text -> RlpToken
lexReservedOp = \case
    "="     -> TokenEquals
    "::"    -> TokenHasType
    "|"     -> TokenPipe

-- | @andBegin@, with the subtle difference that the start code is set
--   /after/ the action
thenBegin :: LexerAction a -> Int -> LexerAction a
thenBegin act c inp l = do
    a <- act inp l
    psLexState . _head .= c
    pure a

thenBeginPush :: LexerAction a -> Int -> LexerAction a
thenBeginPush act c inp l = do
    a <- act inp l
    pushLexState c
    pure a

andBegin :: LexerAction a -> Int -> LexerAction a
andBegin act c inp l = do
    psLexState . _head .= c
    act inp l

beginPush :: Int -> LexerAction (Located RlpToken)
beginPush n _ _ = pushLexState n >> lexToken

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp = case inp ^. aiBytes of
    [] -> do
        (c,t) <- T.uncons (inp ^. aiSource)
        let (b:bs) = encodeChar c
                       -- tail the source
            inp' = inp & aiSource .~ t
                       -- record the excess bytes for successive calls
                       & aiBytes .~ bs
                       -- report the previous char
                       & aiPrevChar .~ c
                       -- update the position
                       & aiPos %~ \ (ln,col,a) ->
                                   if c == '\n'
                                   then (ln+1, 1, a+1)
                                   else (ln, col+1, a+1)
        pure (b, inp')
    
    _ -> Just (head bs, inp')
        where
            (bs, inp') = inp & aiBytes <<%~ drop 1

getInput :: P AlexInput
getInput = use psInput

getLexState :: P Int
getLexState = use (psLexState . singular _head)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = view aiPrevChar

pushLexState :: Int -> P ()
pushLexState n = psLexState %= (n:)

readInt :: Text -> Int
readInt = T.foldl f 0 where
    f n c = 10*n + digitToInt c

constToken :: RlpToken -> LexerAction (Located RlpToken)
constToken t inp l = do
    pos <- use (psInput . aiPos)
    pure (Located (spanFromPos pos l) t)

tokenWith :: (Text -> RlpToken) -> LexerAction (Located RlpToken)
tokenWith tf inp l = do
    pos <- getPos
    let t = tf (T.take l $ inp ^. aiSource)
    pure (Located (spanFromPos pos l) t)

getPos :: P Position
getPos = use (psInput . aiPos)

alexEOF :: P (Located RlpToken)
alexEOF = do
    inp <- getInput
    pos <- getPos
    pure (Located (spanFromPos pos 0) TokenEOF)

runP' :: P a -> Text -> (ParseState, [MsgEnvelope RlpParseError], Maybe a)
runP' p s = runP p st where
    st = initParseState [layout_top,0] s

lexToken :: P (Located RlpToken)
lexToken = do
    inp <- getInput
    c <- getLexState
    st <- use id
    -- traceM $ "st: " <> show st
    case alexScan inp c of
        AlexEOF -> pure $ Located (spanFromPos (inp^.aiPos) 0) TokenEOF
        AlexSkip inp' l -> do
            psInput .= inp'
            lexToken
        AlexToken inp' l act -> do
            psInput .= inp'
            act inp l
        AlexError inp' -> addFatalHere 1 RlpParErrLexical

lexCont :: (Located RlpToken -> P a) -> P a
lexCont = (lexToken >>=)

lexStream :: P [RlpToken]
lexStream = do
    t <- lexToken
    case t of
        Located _ TokenEOF -> pure [TokenEOF]
        Located _ t        -> (t:) <$> lexStream

lexDebug :: (Located RlpToken -> P a) -> P a
lexDebug k = do
    t <- lexToken
    traceM $ "token: " <> show t
    k t

lexTest :: Text -> Maybe [RlpToken]
lexTest s = runP' lexStream s ^. _3

indentLevel :: P Int
indentLevel = do
    pos <- use (psInput . aiPos)
    pure (pos ^. _2)

insertToken :: RlpToken -> P (Located RlpToken)
insertToken t = do
    pos <- use (psInput . aiPos)
    pure (Located (spanFromPos pos 0) t)

popLayout :: P Layout
popLayout = do
    -- traceM "pop layout"
    ctx <- preuse (psLayoutStack . _head)
    psLayoutStack %= (drop 1)
    case ctx of
        Just l      -> pure l
        Nothing     -> error "popLayout: layout stack empty! this is a bug."

pushLayout :: Layout -> P ()
pushLayout l = do
    -- traceM "push layout"
    psLayoutStack %= (l:)

popLexState :: P ()
popLexState = do
    psLexState %= tail

insertSemicolon, insertLBrace, insertRBrace :: P (Located RlpToken)
insertSemicolon = {- traceM "inserting semi"   >> -} insertToken TokenSemicolonV
insertLBrace    = {- traceM "inserting lbrace" >> -} insertToken TokenLBraceV
insertRBrace    = {- traceM "inserting rbrace" >> -} insertToken TokenRBraceV

cmpLayout :: P Ordering
cmpLayout = do
    i <- indentLevel
    ctx <- preuse (psLayoutStack . _head)
    case ctx of
        Just (Implicit n) -> pure (i `compare` n)
        _                 -> pure GT

doBol :: LexerAction (Located RlpToken)
doBol inp l = do
    off <- cmpLayout
    i <- indentLevel
    traceM $ "i: " <> show i
    -- important that we pop the lex state lest we find our lexer diverging
    popLexState
    case off of
        -- the line is aligned with the previous. it therefore belongs to the
        -- same list
        EQ -> insertSemicolon
        -- the line is indented further than the previous, so we assume it is a
        -- line continuation. ignore it and move on!
        GT -> lexToken
        -- the line is indented less than the previous, pop the layout stack and
        -- insert a closing brace.
        LT -> popLayout >> insertRBrace

thenDo :: LexerAction a -> P b -> LexerAction a
thenDo act p inp l = act inp l <* p

explicitLBrace :: LexerAction (Located RlpToken)
explicitLBrace inp l = do
    pushLayout Explicit
    constToken TokenLBrace inp l

explicitRBrace :: LexerAction (Located RlpToken)
explicitRBrace inp l = do
    popLayout
    constToken TokenRBrace inp l

doLayout :: LexerAction (Located RlpToken)
doLayout _ _ = do
    i <- indentLevel
    -- traceM $ "doLayout: i: " <> show i
    pushLayout (Implicit i)
    popLexState
    insertLBrace

programInitState :: Text -> ParseState
programInitState = initParseState [layout_top,0]

}

