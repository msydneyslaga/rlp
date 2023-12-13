{
{-|
Module      : Core.Lex
Description : Lexical analysis for the core language
-}
module Core.Lex
    ( lexCore
    , lexCore'
    , CoreToken(..)
    , SrcError(..)
    , SrcErrorType(..)
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

%wrapper "monad"

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
$nonwhite  = $printable # $white
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

<0>
{
    "("                     { constTok TokenLParen }
    ")"                     { constTok TokenRParen }
    "{"                     { constTok TokenLBrace }
    "}"                     { constTok TokenRBrace }
    ";"                     { constTok TokenSemicolon }
    "@"                     { constTok TokenTypeApp }
    "{-#"                   { constTok TokenLPragma `andBegin` pragma }

    "let"                   { constTok TokenLet }
    "letrec"                { constTok TokenLetrec }
    "of"                    { constTok TokenOf }
    "case"                  { constTok TokenCase }
    "module"                { constTok TokenModule }
    "in"                    { constTok TokenIn }
    "where"                 { constTok TokenWhere }
    "Pack"                  { constTok TokenPack } -- temp

    "\\"                    { constTok TokenLambda }
    "λ"                     { constTok TokenLambda }
    "="                     { constTok TokenEquals }
    "->"                    { constTok TokenArrow }

    @varname                { lexWith TokenVarName }
    @conname                { lexWith TokenConName }
    @varsym                 { lexWith TokenVarSym }
    @consym                 { lexWith TokenConSym }

    @decimal                { lexWith (TokenLitInt . read @Int) }

    $white                  { skip }
    \n                      { skip }
}

<pragma>
{
    "#-}"                   { constTok TokenRPragma `andBegin` 0 }

    $white                  { skip }
    \n                      { skip }

    $nonwhite+              { lexWith TokenWord }
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
               | TokenPack -- temp
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
               | TokenSemicolon
               | TokenTypeApp
               | TokenLPragma
               | TokenRPragma
               | TokenWord String
               | TokenEOF
               deriving Show

data SrcError = SrcError
    { _errSpan       :: (Int, Int, Int)
    , _errSeverity   :: Severity
    , _errDiagnostic :: SrcErrorType
    }
    deriving Show

data SrcErrorType = SrcErrLexical String
                  | SrcErrParse
                  | SrcErrUnknownPragma Name
                  deriving Show

type Lexer = AlexInput -> Int -> Alex (Located CoreToken)

lexWith :: (String -> CoreToken) -> Lexer
lexWith f (AlexPn _ y x,_,_,s) l = pure $ Located y x l (f $ take l s)

-- | The main lexer driver.
lexCore :: String -> RLPC SrcError [Located CoreToken]
lexCore s = case m of
    Left e   -> addFatal err
        where err = SrcError
                { _errSpan       = (0,0,0) -- TODO: location
                , _errSeverity   = Error
                , _errDiagnostic = SrcErrLexical e
                }
    Right ts -> pure ts
    where
        m = runAlex s lexStream

-- | @lexCore@, but the tokens are stripped of location info. Useful for
-- debugging
lexCore' :: String -> RLPC SrcError [CoreToken]
lexCore' s = fmap f <$> lexCore s
    where f (Located _ _ _ t) = t

lexStream :: Alex [Located CoreToken]
lexStream = do
    l <- alexMonadScan
    case l of
        Located _ _ _ TokenEOF  -> pure [l]
        _                       -> (l:) <$> lexStream

data ParseError = ParErrLexical String
                | ParErrParse
                deriving Show

alexEOF :: Alex (Located CoreToken)
alexEOF = Alex $ \ st@(AlexState { alex_pos = AlexPn _ y x }) ->
    Right (st, Located y x 0 TokenEOF)

}

