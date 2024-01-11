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
import Data.Text                (Text)
import Data.Text                qualified as T
import Lens.Micro.Mtl
import Lens.Micro
import Lens.Micro.TH
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
    "{"                 { constToken TokenLBrace }
    "}"                 { constToken TokenRBrace }

<0>
{
    @varname            { tokenWith TokenVarName }
    "="                 { constToken TokenEquals }
}

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
    | TokenEOF
    deriving (Show)

newtype P a = P { runP :: Text -> Either String a }
    deriving (Functor)

data AlexUserState = AlexUserState
    { _ausLayoutStack     :: [Layout]
    }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
    { _ausLayoutStack = []
    }

data Layout = Explicit
            | Implicit Int
            deriving (Show, Eq)

instance Applicative P where
    pure = P . const . Right

    liftA2 = liftM2

instance Monad P where
    m >>= k = P $ \s -> case runP m s of
        Right a -> runP (k a) s
        Left e -> Left e

data Located a = Located AlexPosn a
    deriving (Show)

ausLayoutStack :: Lens' AlexUserState [Layout]
ausLayoutStack = lens _ausLayoutStack
    (\ s l -> s { _ausLayoutStack = l })

lexer :: (Located RlpToken -> P a) -> P a
lexer f = P $ \s -> case m s of
        Right (a,s') -> runP (f a) (s' ^. _4)
        Left  e      -> error (show e)
    where
        m s = runAlex s ((,) <$> alexMonadScan <*> alexGetInput)

lexStream :: P [RlpToken]
lexStream = lexer go where
    go (Located _ TokenEOF) = pure [TokenEOF]
    go (Located _ t)        = (t:) <$!> lexStream

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
    case ctx ^. non (Implicit 0) of
        Implicit n -> pure (n `compare` i)
        Explicit   -> pure GT

doBol :: AlexAction (Located RlpToken)
doBol _ _ = do
    undefined

}

