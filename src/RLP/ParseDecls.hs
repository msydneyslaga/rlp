-- Show Y
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.List                    (foldl1')
import Data.Void
import Data.Char
import Data.Functor
import Data.Functor.Foldable
import Data.HashMap.Strict          qualified as H
import Control.Monad
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
sc = L.space hspace1 (void lineComment) (void blockComment)

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
    [ try $ fmap Y $ U <$> partialExpr1' <*> lexeme infixOp <*> partialExpr'
    , foldl1' papp <$> some partialExpr1
    ]
    where
        partialExpr1' = unY <$> partialExpr1
        partialExpr' = unY <$> partialExpr

        papp :: PartialExpr' -> PartialExpr' -> PartialExpr'
        papp f x = Y . E $ f `AppEF` x

partialExpr1 :: Parser PartialExpr'
partialExpr1 = choice
    [ try $ char '(' *> (hoistY P <$> partialExpr) <* char ')'
    , fmap Y $ varid'
    , fmap Y $ lit'
    ]
    where
        varid' = E . VarEF <$> varid
        lit' = E . LitEF <$> lit

infixOp :: Parser Name
infixOp = symvar <|> symcon

symvar :: Parser Name
symvar = T.pack <$>
    liftA2 (:) (satisfy isVarSym) (many $ satisfy isSym)

symcon :: Parser Name
symcon = T.pack <$>
    liftA2 (:) (char ':') (many $ satisfy isSym)

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

lit :: Parser Lit'
lit = int
    where
        int = IntL <$> L.decimal

----------------------------------------------------------------------------------

-- absolute psycho shit

type PartialDecl' = Decl (Const PartialExpr') Name

newtype Y f = Y (f (Y f))

unY :: Y f -> f (Y f)
unY (Y f) = f

hoistY :: (Functor f) => (forall a. f a -> g a) -> Y f -> Y g
hoistY m (Y f) = Y $ m (hoistY m <$> f)

instance (Show (f (Y f))) => Show (Y f) where
    showsPrec p (Y f) = showsPrec p f

data Partial a = E (RlpExprF Name a)
               | U (Partial a) Name (Partial a)
               | P (Partial a)
               deriving (Show, Functor)

type PartialExpr' = Y Partial

----------------------------------------------------------------------------------

mkOp :: RlpExpr b -> RlpExpr b -> RlpExpr b -> RlpExpr b
mkOp f a b = (f `AppE` a) `AppE` b

