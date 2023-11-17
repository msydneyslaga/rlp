{
module Core.Lex
    ( CoreToken(..)
    , lexCore
    )
    where

import Core.Syntax
import Lens.Micro
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]

$special = [\*\^\%\$#@!\<\>\+\-\=\/&\|\\\.]

$nameTail = [ $alpha $digit \_ \' ]

rlp :-

-- tokens :-
--     $white+                 ;
--     "--" ~$special .*       ;
--     module                  { const TokenModule }
--     where                   { const TokenWhere }
--     let                     { const TokenLet }
--     letrec                  { const TokenLetrec }
--     in                      { const TokenIn }
--     case                    { const TokenCase }
--     of                      { const TokenOf }
--     $digit+                 { TokenLitInt . read @Int }
--     ","                     { const TokenComma }
--     "("                     { const TokenLParen }
--     ")"                     { const TokenRParen }
--     "{"                     { const TokenLBrace }
--     "}"                     { const TokenRBrace }
--     "\\"                    { const TokenLambda }
--     "λ"                     { const TokenLambda }
--     ";"                     { const TokenSemicolon }
--     $special+               { lexSym }
--     $alpha $nameTail*       { TokenName }

<0> \n {begin bol}

<bol>
{
    \n                                    ;
    ()                                    { doBOL }
}

{

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
               | TokenName Name
               | TokenSym Name
               | TokenEquals
               | TokenLParen
               | TokenRParen
               | TokenLBrace
               | TokenRBrace
               | TokenSemicolon
               deriving Show

data LayoutContext = NoLayout
                   | Layout Int

data AlexUserState = AlexUserState
    { _ausContext :: [LayoutContext]
    }

ausContext :: Lens' AlexUserState [LayoutContext]
ausContext = lens _ausContext (\s b -> s { _ausContext = b })

alexInitUserState = AlexUserState
    { _ausContext = []
    }

-- lexCore :: String -> [CoreToken]
lexCore = alexScanTokens

-- lexSym :: String -> CoreToken
-- lexSym "="  = TokenEquals
-- lexSym "\\" = TokenLambda
-- lexSym "->" = TokenArrow
-- lexSym s    = TokenSym s

lexSym = undefined

doBOL = undefined

alexEOF = undefined

alexScanTokens = undefined

}
