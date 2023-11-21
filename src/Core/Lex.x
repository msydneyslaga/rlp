{
{-# LANGUAGE TemplateHaskell #-}
module Core.Lex
    ( lexCore
    , lexCore'
    , CoreToken(..)
    , lexTmp
    , ParserError
    )
    where
import Data.Char (chr)
import Debug.Trace
import Core.Syntax
import Lens.Micro
import Lens.Micro.TH
}

%wrapper "monadUserState"

$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']

$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]

$graphic   = [$small $large $symbol $digit $special \:\"\']

$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$namechar  = [$alpha $digit \']
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
    "letrec"                { constTok TokenLet `andBegin` layout }
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

<0> \n                      { begin bol }

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
data Located a = Located AlexPosn a
    deriving Show

constTok :: t -> AlexInput -> Int -> Alex (Located t)
constTok t (p,_,_,_) _ = pure $ Located p t

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

alexEOF :: Alex (Located CoreToken)
alexEOF = Alex $ \ st@(AlexState { alex_pos = p }) -> Right (st, Located p TokenEOF)

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState [Layout 1]

nestedComment :: Lexer
nestedComment _ _ = undefined

lexStream :: Alex [Located CoreToken]
lexStream = do
    l <- alexMonadScan
    case l of
        Located _ TokenEOF  -> pure [l]
        _                   -> (l:) <$> lexStream

lexCore :: String -> Either String [Located CoreToken]
lexCore s = runAlex s (alexSetStartCode 0 *> lexStream)

lexCore' :: String -> Either String [CoreToken]
lexCore' s = fmap f <$> lexCore s
    where f (Located _ t) = t

lexWith :: (String -> CoreToken) -> Lexer
lexWith f (p,_,_,s) l = pure $ Located p (f $ take l s)

lexToken :: Alex (Located CoreToken)
lexToken = alexMonadScan

getSrcCol :: Alex Int
getSrcCol = Alex $ \ st ->
    let AlexPn _ _ col = alex_pos st
    in Right (st, col)

lbrace :: Lexer
lbrace (p,_,_,_) _ = do
    pushContext NoLayout
    pure $ Located p TokenLBrace

rbrace :: Lexer
rbrace (p,_,_,_) _ = do
    popContext
    pure $ Located p TokenRBrace

insRBraceV :: AlexPosn -> Alex (Located CoreToken)
insRBraceV p = do
    popContext
    pure $ Located p TokenRBraceV

insSemi  :: AlexPosn -> Alex (Located CoreToken)
insSemi p = do
    pure $ Located p TokenSemicolon

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
noBrace (p,_,_,_) l = do
    col <- getSrcCol
    pushContext (Layout col)
    pure $ Located p TokenLBraceV

getOffside :: Alex Ordering
getOffside = do
    ctx <- getContext
    m <- getSrcCol
    case ctx of
        Layout n : _ -> pure $ m `compare` n
        _            -> pure GT

doBol :: Lexer
doBol (p,c,_,s) l = do
    off <- getOffside
    col <- getSrcCol
    case off of
        LT      -> insRBraceV p
        EQ      -> insSemi p
        _       -> lexToken

letin :: Lexer
letin (p,_,_,_) l = do
    popContext
    pure $ Located p TokenIn

lexTmp :: IO [CoreToken]
lexTmp = do
    s <- readFile "/tmp/t.hs"
    case lexCore' s of
        Left e -> error e
        Right a -> pure a

data ParserError

}
