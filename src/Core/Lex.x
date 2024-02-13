{
{-|
Module      : Core.Lex
Description : Lexical analysis for the core language
-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Lex
    ( lexCore
    , lexCoreR
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
import Data.Text            (Text)
import Data.Text            qualified as T
import Data.String          (IsString(..))
import Data.Functor.Identity
import Core.Syntax
import Compiler.RLPC
import Compiler.Types
-- TODO: unify Located definitions
import Compiler.RlpcError
import Lens.Micro
import Lens.Micro.TH
}

%wrapper "monad-strict-text"

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

@alttag    = "<" $digit+ ">"

rlp :-

<0>
{
    "("                     { constTok TokenLParen }
    ")"                     { constTok TokenRParen }
    "{"                     { constTok TokenLBrace }
    "}"                     { constTok TokenRBrace }
    ";"                     { constTok TokenSemicolon }
    "::"                    { constTok TokenHasType }
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

    "\"                     { constTok TokenLambda }
    "λ"                     { constTok TokenLambda }
    "="                     { constTok TokenEquals }
    "->"                    { constTok TokenArrow }

    @alttag                 { lexWith ( TokenAltTag . read @Int . T.unpack
                                      . T.drop 1 . T.init ) }
    @varname                { lexWith TokenVarName }
    @conname                { lexWith TokenConName }
    @varsym                 { lexWith TokenVarSym }
    @consym                 { lexWith TokenConSym }

    @decimal                { lexWith (TokenLitInt . read @Int . T.unpack) }

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

constTok :: t -> AlexInput -> Int -> Alex (Located t)
constTok t (AlexPn _ y x,_,_,_) l = pure $ nolo t

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
               | TokenAltTag Tag
               | TokenEquals
               | TokenLParen
               | TokenRParen
               | TokenLBrace
               | TokenRBrace
               | TokenSemicolon
               | TokenHasType
               | TokenTypeApp
               | TokenLPragma
               | TokenRPragma
               | TokenWord Text
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

lexWith :: (Text -> CoreToken) -> Lexer
lexWith f (AlexPn _ y x,_,_,s) l = pure . nolo . f . T.take l $ s

-- | The main lexer driver.
lexCore :: Text -> RLPC [Located CoreToken]
lexCore s = case m of
    Left e   -> error "core lex error"
    Right ts -> pure ts
    where
        m = runAlex s lexStream

lexCoreR :: forall m. (Applicative m) => Text -> RLPCT m [Located CoreToken]
lexCoreR = hoistRlpcT generalise . lexCore
    where
        generalise :: forall a. Identity a -> m a
        generalise (Identity a) = pure a

-- | @lexCore@, but the tokens are stripped of location info. Useful for
-- debugging
lexCore' :: Text -> RLPC [CoreToken]
lexCore' s = fmap f <$> lexCore s
    where f (Located _ t) = t

lexStream :: Alex [Located CoreToken]
lexStream = do
    l <- alexMonadScan
    case l of
        Located _ TokenEOF -> pure [l]
        _                  -> (l:) <$> lexStream

data ParseError = ParErrLexical String
                | ParErrParse
                deriving Show

-- TODO:
instance IsRlpcError SrcError where
    liftRlpcError = Text . pure . T.pack . show

-- TODO:
instance IsRlpcError ParseError where
    liftRlpcError = Text . pure . T.pack . show

alexEOF :: Alex (Located CoreToken)
alexEOF = Alex $ \ st@(AlexState { alex_pos = AlexPn _ y x }) ->
    Right (st, nolo $ TokenEOF)

}

