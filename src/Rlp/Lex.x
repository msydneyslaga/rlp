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
    )
    where
import Control.Monad
import Data.Functor.Identity
import Core.Syntax              (Name)
import Data.Monoid              (First)
import Data.Maybe
import Data.Text                (Text)
import Data.Text                qualified as T
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
    "{"                 { explicitLBrace }
    "}"                 { explicitRBrace }

<0>
{
    $whitechar+         ;
    \n                  ;
    "{"                 { expectLBrace }
}

<one>
{
    \n                  { begin bol }
    @varname            { tokenWith TokenVarName }
    "="                 { constToken TokenEquals }
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
    -- 'virtual' control symbols, implicitly inserted by the lexer
    | TokenSemicolonV
    | TokenLBraceV
    | TokenRBraceV
    | TokenEOF
    deriving (Show)

newtype P a = P {
        runP :: AlexUserState -> Text -> Either String (AlexUserState, a)
    }
    deriving (Functor)

runPInit :: P a -> Text -> Either String (AlexUserState, a)
runPInit p = runP p alexInitUserState

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

instance Applicative P where
    pure a = P $ \st _ -> Right (st,a)

    liftA2 = liftM2

instance Monad P where
    m >>= k = P $ \st s -> case runP m st s of
        Right (st',a) -> runP (k a) st' s
        Left e        -> Left e

data Located a = Located AlexPosn a
    deriving (Show)

ausLayoutStack :: Lens' AlexUserState [(Layout, Int)]
ausLayoutStack = lens _ausLayoutStack
    (\ s l -> s { _ausLayoutStack = l })

lexer :: (Located RlpToken -> P a) -> P a
lexer f = P $ \st s -> case m st s of
        Right (a,st',s') -> runP (f a) st' (s' ^. _4)
        Left  e          -> error (show e)
    where
        m st s = runAlex s
            ((,,) <$> (alexSetUserState st *> alexMonadScan)
                  <*> alexGetUserState
                  <*> alexGetInput)

lexStream :: P [RlpToken]
lexStream = lexer go where
    go (Located _ TokenEOF) = pure [TokenEOF]
    go (Located _ t)        = (t:) <$!> lexStream

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
insertSemicolon = insertToken TokenSemicolonV
insertLBrace = insertToken TokenLBraceV
insertRBrace = insertToken TokenRBraceV

-- pop the layout stack and jump to the popped return code
popLayout :: Alex ()
popLayout = do
    ctx <- preuseAus (ausLayoutStack . _head)
    modifyingAus ausLayoutStack (drop 1)
    case ctx of
        Just (l,c)  -> alexSetStartCode c
        Nothing     -> pure ()

pushLayout :: Layout -> Alex ()
pushLayout l = do
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
        LT -> popLayout >> insertRBrace >> alexSetStartCode 0 >> lexToken

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

expectLBrace :: AlexAction (Located RlpToken)
expectLBrace _ _ = do
    off <- cmpLayout

}

