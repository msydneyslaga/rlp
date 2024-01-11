{
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
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
import Data.Text                (Text)
import Data.Text                qualified as T
import Lens.Micro
}

%wrapper "monad-strict-text"

$whitechar = [ \t\n\r\f\v]

rlp :-
    
    -- skip whitespace
    $white+             ;
    -- TODO: don't treat operators like (-->) as comments
    "--".*              ;
    ";"                 { constToken TokenSemicolon }
    "{"                 { constToken TokenLBrace }
    "}"                 { constToken TokenRBrace }

<0>
{
    "a" { const $ const $ pure $ Located (AlexPn 0 0 0) (TokenVarName "a") }
}

{

constToken :: RlpToken -> AlexAction (Located RlpToken)
constToken t inp _ = pure $ Located (inp ^. _1) t

alexEOF :: Alex (Located RlpToken)
alexEOF = do
    inp <- alexGetInput
    pure (Located (inp ^. _1) TokenEOF)

data RlpToken = TokenEquals
              | TokenLitInt Int
              | TokenVarName Name
              | TokenConName Name
              | TokenVarSym Name
              | TokenConSym Name
              | TokenData
              | TokenPipe
              -- syntax control
              | TokenSemicolon
              | TokenLBrace
              | TokenRBrace
              | TokenEOF
              deriving (Show)

newtype P a = P { runP :: PState -> Text -> Either String a }
    deriving (Functor)

data PState = PState
    { psLayoutStack     :: [Layout]
    }

data Layout = ExplicitLayout
            | ImplicitLayout Int
            deriving (Show)

instance Applicative P where
    pure = P . const . const . Right

    liftA2 = liftM2

instance Monad P where
    m >>= k = P $ \st s -> case runP m st s of
        Right a -> runP (k a) st s
        Left e -> Left e

data Located a = Located AlexPosn a
    deriving (Show)

lexer :: (Located RlpToken -> P a) -> P a
lexer f = P $ \st s -> case m s of
        Right (a,s') -> runP (f a) st (s' ^. _4)
        Left  e      -> error (show e)
    where
        m s = runAlex s ((,) <$> alexMonadScan <*> alexGetInput)

lexStream :: P [RlpToken]
lexStream = lexer go where
    go (Located _ TokenEOF) = pure [TokenEOF]
    go (Located _ t)        = (t:) <$> lexStream

}
