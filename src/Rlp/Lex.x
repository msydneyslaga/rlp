{
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

begin :: Int -> LexerAction a
begin = undefined

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp = case inp ^. aiBytes of
    [] -> do
        (c,t) <- T.uncons (inp ^. aiSource)
        let (b:bs) = encodeChar c
            inp' = inp & aiSource .~ t
                       & aiBytes .~ bs
                       & aiPrevChar .~ c
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
    , _psLexState = [one,bol,0]
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
    case alexScan inp c of
        AlexEOF -> pure $ Located (inp ^. aiPos, 0) TokenEOF
        AlexToken inp' l act -> do
            psInput .= inp'
            traceM $ "l: " <> show l
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

lexTest :: Text -> Either String [RlpToken]
lexTest = undefined

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
    modifying psLayoutStack (drop 1)
    case ctx of
        Just l      -> pure l
        Nothing     -> error "uhh"

insertSemicolon, insertLBrace, insertRBrace :: P (Located RlpToken)
insertSemicolon = traceM "inserting semi"   >> insertToken TokenSemicolonV
insertLBrace    = traceM "inserting lbrace" >> insertToken TokenLBraceV
insertRBrace    = traceM "inserting rbrace" >> insertToken TokenRBraceV

cmpLayout :: P Ordering
cmpLayout = do
    i <- indentLevel
    ctx <- preuse (psLayoutStack . _head)
    case ctx ^. non (Implicit 1) of
        Implicit n -> pure (i `compare` n)
        Explicit   -> pure GT

doBol :: LexerAction (Located RlpToken)
doBol inp l = do
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
        LT -> popLayout >> insertRBrace

}

