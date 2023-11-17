{
{-# LANGUAGE TemplateHaskell #-}
module Core.Lex
    ( lexCore
    , lexCore'
    , CoreToken(..)
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
    "module"                { constTok TokenModule }
    "let"                   { constTok TokenLet `andBegin` layout }
    "letrec"                { constTok TokenLet `andBegin` layout }
    "case"                  { constTok TokenCase }
    "of"                    { constTok TokenOf `andBegin` layout }
    "in"                    { constTok TokenIn }
    "where"                 { constTok TokenWhere }
}

-- reserved symbols
<0>
{
    "="                     { constTok TokenEquals }
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
    ()                      { doBOL }
}

<layout>
{
    -- TODO: does not respect comments nor pragmas
    \{                      { doLayoutBrace }
    \n                      { skip }
    ()                      { newLayoutContext }
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
               | TokenName Name -- temp
               | TokenVarSym Name
               | TokenConSym Name
               | TokenSym Name -- temp
               | TokenEquals
               | TokenLParen
               | TokenRParen
               | TokenLBrace
               | TokenRBrace
               | TokenSemicolon
               | TokenEOF
               deriving Show

data LayoutContext = Layout Int
                   | NoLayout

data AlexUserState = AlexUserState
    { _ausContext :: [LayoutContext]
    , _ausStack   :: [Int]
    }

ausContext :: Lens' AlexUserState [LayoutContext]
ausContext f (AlexUserState ctx stk)
  = fmap
      (\a -> AlexUserState a stk) (f ctx)
{-# INLINE ausContext #-}

ausStack :: Lens' AlexUserState [Int]
ausStack f (AlexUserState ctx stk)
  = fmap
      (\a -> AlexUserState ctx a) (f stk)
{-# INLINE ausStack #-}

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
alexInitUserState = AlexUserState [] [bol,0]

nestedComment :: Lexer
nestedComment _ _ = undefined

lexStream :: Alex [Located CoreToken]
lexStream = do
    l <- alexMonadScan
    case l of
        Located _ TokenEOF  -> pure [l]
        _                   -> (l:) <$> lexStream

lexCore :: String -> Either String [Located CoreToken]
lexCore s = runAlex s lexStream

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

doLayoutBrace :: Lexer
doLayoutBrace (p,_,_,s) _ = undefined

lbrace :: Lexer
lbrace (p,_,_,_) _ = do
    pushContext NoLayout
    pure $ Located p TokenLBrace

rbrace :: Lexer
rbrace (p,_,_,_) _ = do
    popContext
    pure $ Located p TokenRBrace

setLexState :: Int -> Alex ()
setLexState n = Alex $ \st -> Right (st { alex_scd = n }, ())

modifyUst :: (AlexUserState -> AlexUserState) -> Alex ()
modifyUst f = do
    st <- alexGetUserState
    alexSetUserState $ f st

getUst :: Alex AlexUserState
getUst = alexGetUserState

pushLexState :: Int -> Alex ()
pushLexState n = modifyUst (ausStack %~ (n:)) *> setLexState n

popLexState :: Alex Int
popLexState = do
    modifyUst (ausStack %~ drop 1)
    ust <- getUst
    let s = case ust ^. ausStack of
            (a:_) -> a
            _     -> 0
    setLexState s
    pure s

newLayoutContext :: Lexer
newLayoutContext (p,_,_,_) _ = do
    _ <- popLexState
    ctx <- getContext
    off <- getSrcCol
    case ctx of
        Layout prev : _ | off <= prev
            -> error $ show prev
        _ -> do
            pushContext $ Layout off
            pure $ Located p TokenLBrace

doBOL :: Lexer
doBOL = undefined

}
