{
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.Lex
    ( P(..)
    , RlpToken(..)
    , Located(..)
    , lexToken
    , lexerCont
    )
    where
import Codec.Binary.UTF8.String (encodeChar)
import Control.Monad
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

<0>
{
    \n                  { beginPush bol }
    @varname            { tokenWith TokenVarName }
    @digits             { tokenWith (TokenLitInt . readInt) }
    "="                 { constToken TokenEquals }
}

-- control characters
<0>
{
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

<layout_top>
{
    \n                  ;
    "{"                 { explicitLBrace `thenDo` popLexState }
    ()                  { doLayout }
}

{

-- | @andBegin@, with the subtle difference that the start code is set
--   /after/ the action
thenBegin :: LexerAction a -> Int -> LexerAction a
thenBegin act c inp l = do
    a <- act inp l
    psLexState . _head .= c
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
                       & aiPos %~ \ (ln,col) ->
                                   if (inp ^. aiPrevChar) == '\n'
                                   then (ln+1,1)
                                   else (ln,col+1)
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
readInt = T.foldr f 0 where
    f c n = digitToInt c + 10*n

constToken :: RlpToken -> LexerAction (Located RlpToken)
constToken t inp l = do
    pos <- use (psInput . aiPos)
    pure (Located (pos,l) t)

tokenWith :: (Text -> RlpToken) -> LexerAction (Located RlpToken)
tokenWith tf inp l = do
    pos <- getPos
    let t = tf (T.take l $ inp ^. aiSource)
    pure (Located (pos,l) t)

getPos :: P Position
getPos = use (psInput . aiPos)

alexEOF :: P (Located RlpToken)
alexEOF = do
    inp <- getInput
    pure (Located undefined TokenEOF)

execP :: P a -> ParseState -> Maybe a
execP p st = runP p st & snd

execP' :: P a -> Text -> Maybe a
execP' p s = execP p st where
    st = initParseState s

initParseState :: Text -> ParseState
initParseState s = ParseState
    { _psLayoutStack = []
    -- IMPORTANT: the initial state is `bol` to begin the top-level layout,
    -- which then returns to state 0 which continues the normal lexing process.
    , _psLexState = [layout_top,0]
    , _psInput = initAlexInput s
    }

initAlexInput :: Text -> AlexInput
initAlexInput s = AlexInput
    { _aiPrevChar   = '\0'
    , _aiSource     = s
    , _aiBytes      = []
    , _aiPos        = (1,1)
    }

lexToken :: P (Located RlpToken)
lexToken = do
    inp <- getInput
    c <- getLexState
    st <- use id
    traceM $ "st: " <> show st
    case alexScan inp c of
        AlexEOF -> pure $ Located (inp ^. aiPos, 0) TokenEOF
        AlexSkip inp' l -> do
            psInput .= inp'
            lexToken
        AlexToken inp' l act -> do
            psInput .= inp'
            traceShowM inp'
            act inp l

lexerCont :: (Located RlpToken -> P a) -> P a
lexerCont = undefined

lexStream :: P [RlpToken]
lexStream = do
    t <- lexToken
    case t of
        Located _ TokenEOF -> pure [TokenEOF]
        Located _ t        -> (t:) <$> lexStream

lexTest :: Text -> Maybe [RlpToken]
lexTest s = execP' lexStream s

indentLevel :: P Int
indentLevel = do
    pos <- use (psInput . aiPos)
    pure (pos ^. _2)

insertToken :: RlpToken -> P (Located RlpToken)
insertToken t = do
    pos <- use (psInput . aiPos)
    pure (Located (pos, 0) t)

popLayout :: P Layout
popLayout = do
    traceM "pop layout"
    ctx <- preuse (psLayoutStack . _head)
    psLayoutStack %= (drop 1)
    case ctx of
        Just l      -> pure l
        Nothing     -> error "uhh"

pushLayout :: Layout -> P ()
pushLayout l = do
    traceM "push layout"
    psLayoutStack %= (l:)

popLexState :: P ()
popLexState = do
    psLexState %= tail

insertSemicolon, insertLBrace, insertRBrace :: P (Located RlpToken)
insertSemicolon = traceM "inserting semi"   >> insertToken TokenSemicolonV
insertLBrace    = traceM "inserting lbrace" >> insertToken TokenLBraceV
insertRBrace    = traceM "inserting rbrace" >> insertToken TokenRBraceV

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
    pushLayout (Implicit i)
    popLexState
    insertLBrace

}

