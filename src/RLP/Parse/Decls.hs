{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Rlp.Parse.Decls
    (
    )
    where
----------------------------------------------------------------------------------
import Control.Monad
import Control.Monad.State
import Text.Megaparsec              hiding (State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer   qualified as L
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.List                    (foldl1')
import Data.Char
import Data.Functor
import Data.Functor.Const
import Data.Fix                     hiding (cata)
import Lens.Micro
import Rlp.Parse.Types
import Rlp.Syntax
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
    [ try $ fmap Fix $ mkB <$> partialExpr1' <*> lexeme infixOp <*> partialExpr'
    , foldl1' papp <$> some partialExpr1
    ]
    where
        mkB a f b = B f a b
        partialExpr1' = unFix <$> partialExpr1
        partialExpr' = unFix <$> partialExpr

        papp :: PartialExpr' -> PartialExpr' -> PartialExpr'
        papp f x = Fix . E $ f `AppEF` x

partialExpr1 :: Parser PartialExpr'
partialExpr1 = choice
    [ try $ char '(' *> (hoistFix P <$> partialExpr) <* char ')'
    , fmap Fix $ varid'
    , fmap Fix $ lit'
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

type PartialE = Partial RlpExpr'

-- complete :: OpTable -> Fix Partial -> RlpExpr'
complete :: OpTable -> PartialExpr' -> RlpExpr'
complete pt = let ?pt = pt in cata completePartial

completePartial :: PartialE -> RlpExpr'
completePartial (E e)        = completeRlpExpr e
completePartial p@(B o l r)  = completeB (build p)
completePartial (P e)        = completePartial e

completeRlpExpr :: RlpExprF' RlpExpr' -> RlpExpr'
completeRlpExpr = embed

completeB :: PartialE -> RlpExpr'
completeB = build

build :: PartialE -> PartialE
build e = go id e (rightmost e) where
    rightmost :: Partial -> Partial
    rightmost (B _ _ _) = rightmost r
    rightmost (E n)     = undefined

    go :: (?pt :: OpTable)
       => (PartialE -> PartialE)
       -> PartialE -> PartialE -> PartialE
    go f p@(WithPrec o _ r) = case r of
            E _     -> mkHole o (f . f')
            P _     -> undefined
            B _ _ _ -> go (mkHole o (f . f')) r
        where f' r' = p & pR .~ r'

mkHole :: (?pt :: OpTable)
       => OpInfo
       -> (PartialE -> PartialE)
       -> PartialE
       -> PartialE
mkHole = undefined

