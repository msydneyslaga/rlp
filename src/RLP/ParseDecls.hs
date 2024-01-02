-- Show Y
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Rlp.ParseDecls
    (
    )
    where
----------------------------------------------------------------------------------
import Rlp.Syntax
import Text.Megaparsec              hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer   qualified as L
import Data.Functor.Const
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.Void
import Data.Char
import Data.Functor
import Data.HashMap.Strict          qualified as H
import Control.Monad
import Core.Syntax
import Control.Monad.State
----------------------------------------------------------------------------------

type Parser = ParsecT Void Text (State ParserState)

data ParserState = ParserState
    { _psPrecTable :: PrecTable
    }
    deriving Show

type PrecTable = H.HashMap Name (Assoc, Int)

----------------------------------------------------------------------------------

parseTest' :: (Show a) => Parser a -> Text -> IO ()
parseTest' p s = case runState (runParserT p "test" s) init of
    (Left e, _)   -> putStr (errorBundlePretty e)
    (Right x, st) -> print st *> print x
    where
        init = ParserState mempty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space space1 (void lineComment) (void blockComment)

-- TODO: return comment text
-- TODO: '---' should not start a comment
lineComment :: Parser Text
lineComment = L.skipLineComment "--" $> "<unimpl>"

-- TODO: return comment text
blockComment :: Parser Text
blockComment = L.skipBlockCommentNested "{-" "-}" $> "<unimpl>"

decl :: Parser PartialDecl'
decl = choice
    [ funD
    , tySigD
    , dataD
    , infixD
    ]

funD :: Parser PartialDecl'
funD = FunD <$> varid <*> many pat1 <*> (symbol "=" *> fmap Const partialExpr)

partialExpr :: Parser PartialExpr'
partialExpr = choice
    [ fmap Y $ U <$> varid' <*> lexeme infixOp <*> varid'
    ]
    where varid' = E . VarEF <$> varid


infixOp :: Parser Name
infixOp = symvar <|> symcon

symvar :: Parser Name
symvar = T.pack <$>
    liftA2 (:) (satisfy isVarSym) (many $ satisfy isSym)

symcon :: Parser Name
symcon = T.pack <$>
    liftA2 (:) (char ':') (many $ satisfy isSym)

-- partialExpr :: Parser (Const Text a)
-- partialExpr = fmap Const $ L.lineFold w $ \w' ->
--     try w' <> w
--     where
--         w = L.space eat (void lineComment) (void blockComment)
--         eat = void . some $ satisfy (not . isSpace)

pat1 :: Parser Pat'
pat1 = VarP <$> varid

varid :: Parser VarId
varid = NameVar <$> lexeme namevar
    <|> SymVar <$> lexeme (char '(' *> symvar <* char ')')
    <?> "variable identifier"
    where
        namevar = T.pack <$>
            liftA2 (:) (satisfy isLower) (many $ satisfy isNameTail)

        isNameTail c = isAlphaNum c
                    || c == '\''
                    || c == '_'

isVarSym :: Char -> Bool
isVarSym = (`T.elem` "\\!#$%&*+./<=>?@^|-~")

isSym :: Char -> Bool
isSym c = c == ':' || isVarSym c

infixD = undefined

tySigD = undefined
dataD = undefined

----------------------------------------------------------------------------------

-- absolute psycho shit

type PartialDecl' = Decl (Const PartialExpr') Name

newtype Y f = Y (f (Y f))

instance (Show (f (Y f))) => Show (Y f) where
    showsPrec p (Y f) = showsPrec p f

data Partial a = E (RlpExprF Name a)
               | U (Partial a) Name (Partial a)
               deriving Show

type PartialExpr' = Y Partial

