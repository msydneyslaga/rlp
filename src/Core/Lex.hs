{-|
Module      : Core.Lex
Description : Core language lexer
-}
module Core.Lex
    ( CoreToken
    , Result
    , lexCore
    )
    where
----------------------------------------------------------------------------------
import Control.Parser
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Core.Syntax              (Name)
----------------------------------------------------------------------------------

type CoreLexer = ParserT String Result

data Result a = Success a
              | Error String Int Int
              deriving (Show)

-- TODO: whitespace-sensitive layout
data CoreToken = TokLitInt Int
               | TokEquals
               | TokLBrace
               | TokRBrace
               | TokSemicolon
               | TokLParen
               | TokRParen
               | TokLambda
               | TokArrow
               | TokCase
               | TokOf
               | TokLet
               | TokLetRec
               | TokIn
               | TokCName Name
               | TokName Name
               deriving (Show)

instance Functor Result where
    fmap f (Success a)   = Success (f a)
    fmap _ (Error s l c) = Error s l c

instance Foldable Result where
    foldr f z (Success a)   = a `f` z
    foldr _ z (Error _ _ _) = z

instance Traversable Result where
    traverse k (Success a)   = fmap Success (k a)
    traverse _ (Error s l c) = pure $ Error s l c

instance Applicative Result where
    pure = Success

    liftA2 f (Success a)   (Success b)   = Success $ f a b
    liftA2 _ (Error s l c) _             = Error s l c
    liftA2 _ _             (Error s l c) = Error s l c

instance Alternative Result where
    empty = Error "unknown failure" 0 0

    (Success a) <|> _ = Success a
    _           <|> b = b

instance Monad Result where
    Success a   >>= k = k a
    Error s l c >>= _ = Error s l c

instance MonadPlus Result

instance MonadFail Result where
    fail s = Error s 0 0

----------------------------------------------------------------------------------

lexCore :: String -> Result [CoreToken]
lexCore = fmap snd . runParserT (many (token <* spaces))

token :: CoreLexer CoreToken
token = litInt
    <|> lbrace
    <|> rbrace
    <|> semicolon
    <|> lparen
    <|> rparen
    <|> equals
    <|> lambda
    <|> arrow
    <|> _case
    <|> _of
    <|> _let
    <|> letrec
    <|> _in
    <|> cName
    <|> name

----------------------------------------------------------------------------------

litInt, equals, lparen, rparen, lambda,
    arrow, _case, _of, _let, letrec, _in, cName, name :: CoreLexer CoreToken

litInt = TokLitInt . value <$> some (satisfy isDigit)
    where
        value = foldl (\acc a -> 10*acc + digitToInt a) 0

semicolon = (semis <|> nls)        $> TokSemicolon
    where
        nls = head <$> some (char '\n')
        semis = char ';' <* many (char '\n')
equals = char '='                  $> TokEquals
lbrace = char '{'                  $> TokLBrace
rbrace = char '}'                  $> TokRBrace
lparen = char '('                  $> TokLParen
rparen = char ')'                  $> TokRParen
lambda = (char '\\' <|> char 'λ')  $> TokLambda
arrow  = string "->"               $> TokArrow
_case  = string "case"             $> TokCase
_of = string "of"                  $> TokOf
_let = string "let"                $> TokLet
letrec = string "letrec"           $> TokLetRec
_in = string "in"                  $> TokIn

cName = TokCName <$> ((:) <$> cNameHead <*> properNameTail)
    where cNameHead = satisfy isUpper

name = some (satisfy p) <&> TokName
    where p c = not (isSpace c) && c `notElem` ";{}"

properName :: CoreLexer Name
properName = (:) <$> nameHead <*> properNameTail
    where nameHead = satisfy isLetter

properNameTail :: CoreLexer Name
properNameTail = many . satisfy $ \c ->
    isLetter c || isDigit c || c == '_'

