{
-- TODO: layout semicolons are not inserted at EOf.
{-# LANGUAGE TemplateHaskell #-}
module Core.Lex
    ( lexCore
    , lexCore'
    , CoreToken(..)
    , ParseError(..)
    , Located(..)
    , AlexPosn(..)
    )
    where
import Data.Char (chr)
import Debug.Trace
import Core.Syntax
import Compiler.RLPC
import Lens.Micro
import Lens.Micro.TH
}

%wrapper "monadUserState"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\{\}]

$digit     = 0-9

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$namechar  = [$alpha $digit \' \#]
$symchar   = [$symbol \:]
$nl        = [\n\r]
$white_no_nl = $white # $nl

@reservedid = 
    case|data|do|import|in|let|letrec|module|of|where

@reservedop =
    "=" | \\ | "->"

@varname   = $small $namechar*
@conname   = $large $namechar*
@varsym    = $symbol $symchar*
@consym    = \: $symchar*

@decimal = $digit+

rlp :-

-- everywhere: skip whitespace
$white_no_nl+           { skip }

-- TODO: `--` could begin an operator
"--"[^$nl]*             { skip }
"--"\-*[^$symbol].*     { skip }

"{-"                        { nestedComment }

-- syntactic symbols
<0>
{
    "("                     { constTok TokenLParen }
    ")"                     { constTok TokenRParen }
    "{"                     { lbrace }
    "}"                     { rbrace }
    ";"                     { constTok TokenSemicolon }
    ","                     { constTok TokenComma }
}

-- keywords
-- see commentary on the layout system
<0>
{
    "let"                   { constTok TokenLet `andBegin` layout }
    "letrec"                { constTok TokenLetrec `andBegin` layout }
    "of"                    { constTok TokenOf `andBegin` layout }
    "case"                  { constTok TokenCase }
    "module"                { constTok TokenModule }
    "in"                    { letin }
    "where"                 { constTok TokenWhere `andBegin` layout }
}

-- reserved symbols
<0>
{
    "="                     { constTok TokenEquals }
    "->"                    { constTok TokenArrow }
}

-- identifiers
<0>
{
    -- TODO: qualified names
    @varname                { lexWith TokenVarName }
    @conname                { lexWith TokenConName }
    @varsym                 { lexWith TokenVarSym }
}

-- literals
<0>
{
    @decimal                { lexWith (TokenLitInt . read @Int) }
}

<0> \n                      { begin bol }

<initial>
{
    $white                  { skip }
    \n                      { skip }
    ()                      { topLevelOff `andBegin` 0 }
}

<bol>
{
    \n                      { skip }
    ()                      { doBol `andBegin` 0 }
}

<layout>
{
    $white                  { skip }
    \{                      { lbrace `andBegin` 0 }
    ()                      { noBrace `andBegin` 0 }
}

{
data Located a = Located Int Int Int a
    deriving Show

constTok :: t -> AlexInput -> Int -> Alex (Located t)
constTok t (AlexPn _ y x,_,_,_) l = pure $ Located y x l t

data CoreToken = TokenLet
               | TokenLetrec
               | TokenIn
               | TokenModule
               | TokenWhere
               | TokenComma
               | TokenCase
               | TokenOf
               | TokenLambda
               | TokenArrow
               | TokenLitInt Int
               | TokenVarName Name
               | TokenConName Name
               | TokenVarSym Name
               | TokenConSym Name
               | TokenEquals
               | TokenLParen
               | TokenRParen
               | TokenLBrace
               | TokenRBrace
               | TokenLBraceV -- virtual brace inserted by layout
               | TokenRBraceV -- virtual brace inserted by layout
               | TokenIndent Int
               | TokenDedent Int
               | TokenSemicolon
               | TokenEOF
               deriving Show

data LayoutContext = Layout Int
                   | NoLayout
                   deriving Show

data AlexUserState = AlexUserState
    { _ausContext :: [LayoutContext]
    }

ausContext :: Lens' AlexUserState [LayoutContext]
ausContext f (AlexUserState ctx)
  = fmap
      (\a -> AlexUserState a) (f ctx)
{-# INLINE ausContext #-}

pushContext :: LayoutContext -> Alex ()
pushContext c = do
    st <- alexGetUserState
    alexSetUserState $ st { _ausContext = c : _ausContext st }

popContext :: Alex ()
popContext = do
    st <- alexGetUserState
    alexSetUserState $ st { _ausContext = drop 1 (_ausContext st) }

getContext :: Alex [LayoutContext]
getContext = do
    st <- alexGetUserState
    pure $ _ausContext st

type Lexer = AlexInput -> Int -> Alex (Located CoreToken)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState []

nestedComment :: Lexer
nestedComment _ _ = undefined

lexStream :: Alex [Located CoreToken]
lexStream = do
    l <- alexMonadScan
    case l of
        Located _ _ _ TokenEOF  -> pure [l]
        _                       -> (l:) <$> lexStream

-- | The main lexer driver.
lexCore :: String -> RLPC ParseError [Located CoreToken]
lexCore s = case m of
    Left e   -> addFatal err
        where err = SrcError
                { _errSpan       = (0,0,0) -- TODO: location
                , _errSeverity   = Error
                , _errDiagnostic = ParErrLexical e
                }
    Right ts -> pure ts
    where
        m = runAlex s (alexSetStartCode initial *> lexStream)

-- | @lexCore@, but the tokens are stripped of location info. Useful for
-- debugging
lexCore' :: String -> RLPC ParseError [CoreToken]
lexCore' s = fmap f <$> lexCore s
    where f (Located _ _ _ t) = t

data ParseError = ParErrLexical String
                | ParErrParse
                deriving Show

lexWith :: (String -> CoreToken) -> Lexer
lexWith f (AlexPn _ y x,_,_,s) l = pure $ Located y x l (f $ take l s)

lexToken :: Alex (Located CoreToken)
lexToken = alexMonadScan

getSrcCol :: Alex Int
getSrcCol = Alex $ \ st ->
    let AlexPn _ _ col = alex_pos st
    in Right (st, col)

lbrace :: Lexer
lbrace (AlexPn _ y x,_,_,_) l = do
    pushContext NoLayout
    pure $ Located y x l TokenLBrace

rbrace :: Lexer
rbrace (AlexPn _ y x,_,_,_) l = do
    popContext
    pure $ Located y x l TokenRBrace

insRBraceV :: AlexPosn -> Alex (Located CoreToken)
insRBraceV (AlexPn _ y x) = do
    popContext
    pure $ Located y x 0 TokenRBraceV

insSemi  :: AlexPosn -> Alex (Located CoreToken)
insSemi (AlexPn _ y x) = do
    pure $ Located y x 0 TokenSemicolon

modifyUst :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUst f = do
    st <- alexGetUserState
    alexSetUserState $ f st

getUst :: Alex AlexUserState
getUst = alexGetUserState

newLayoutContext :: Lexer
newLayoutContext (p,_,_,_) _ = do
    undefined

noBrace :: Lexer
noBrace (AlexPn _ y x,_,_,_) l = do
    col <- getSrcCol
    pushContext (Layout col)
    pure $ Located y x l TokenLBraceV

getOffside :: Alex Ordering
getOffside = do
    ctx <- getContext
    m <- getSrcCol
    case ctx of
        Layout n : _ -> pure $ m `compare` n
        _            -> pure GT

doBol :: Lexer
doBol (p,c,_,s) _ = do
    off <- getOffside
    case off of
        LT      -> insRBraceV p
        EQ      -> insSemi p
        _       -> lexToken

letin :: Lexer
letin (AlexPn _ y x,_,_,_) l = do
    popContext
    pure $ Located y x l TokenIn

topLevelOff :: Lexer
topLevelOff = noBrace

alexEOF :: Alex (Located CoreToken)
alexEOF = Alex $ \ st@(AlexState { alex_pos = AlexPn _ y x }) ->
    Right (st, Located y x 0 TokenEOF)

}
