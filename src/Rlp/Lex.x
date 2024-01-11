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
import Data.Functor.Identity
import Core.Syntax              (Name)
import Data.Text                (Text)
import Data.Text                qualified as T
import Lens.Micro
}

%wrapper "monad-strict-text"

rlp :-
<0>
{
    "a" { const $ const $ pure $ Located (AlexPn 0 0 0) (TokenVarName "a") }
    "" { undefined }
}

{

alexEOF :: Alex a
alexEOF = undefined

data RlpToken = TokenEquals
              | TokenLitInt Int
              | TokenVarName Name
              | TokenConName Name
              | TokenVarSym Name
              | TokenConSym Name
              | TokenData
              | TokenPipe
              | TokenEOF
              deriving (Show)

newtype P a = P { runP :: Text -> Either String a }
    deriving (Functor)

instance Applicative P where
    pure = P . const . Right

    liftA2 f p q = P $ \s -> undefined

instance Monad P where
    m >>= k = P $ \s -> case runP m s of
        Right a -> runP (k a) s
        Left e -> Left e

data Located a = Located AlexPosn a
    deriving (Show)

lexer :: (Located RlpToken -> P a) -> P a
lexer f = P $ \s -> case runAlex s ((,) <$> alexMonadScan <*> alexGetInput) of
    Right (a,s') -> runP (f a) (s' ^. _4)

}
