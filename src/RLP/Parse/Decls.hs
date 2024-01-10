{-# LANGUAGE RecursiveDo #-}
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
import Data.HashMap.Strict          qualified as H
import Data.Maybe                   (maybeToList)
import Data.List                    (foldl1')
import Data.Char
import Data.Functor
import Data.Functor.Const
import Data.Fix                     hiding (cata)
import Lens.Micro
import Lens.Micro.Platform
import Rlp.Parse.Types
import Rlp.Parse.Utils
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

flexeme :: Parser a -> Parser a
flexeme p = L.lineFold scn $ \sc' -> L.lexeme sc' p

symbol :: Text -> Parser Text
symbol = L.symbol sc

sc :: Parser ()
sc = L.space hspace1 (void lineComment) (void blockComment)

scn :: Parser ()
scn = L.space space1 (void lineComment) (void blockComment)

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
funD = FunD <$> lexeme varid <*> params <*> (symbol "=" *> body) <*> whereClause
    where
        params = many pat1
        body = fmap Const partialExpr

whereClause :: Parser Where'
whereClause = optionalList $
        lexeme "where" *> pure
            [ FunB "fixme" [] (VarE "fixme")
            ]

standalonePartialExpr :: Parser PartialExpr'
standalonePartialExpr = standaloneOf partialExpr

standaloneOf :: Parser a -> Parser a
standaloneOf = (<* eof)

partialExpr :: Parser PartialExpr'
partialExpr = choice
    [ try application
    , Fix <$> infixExpr
    ]
    <?> "expression"
    where
        application = foldl1' mkApp <$> some partialExpr1
        infixExpr = mkB <$> partialExpr1' <*> infixOp' <*> partialExpr'

        mkB a f b = B f a b
        partialExpr1' = unFix <$> partialExpr1
        partialExpr' = unFix <$> partialExpr
        infixOp' = lexeme infixOp

        mkApp :: PartialExpr' -> PartialExpr' -> PartialExpr'
        mkApp f x = Fix . E $ f `AppEF` x

partialExpr1 :: Parser PartialExpr'
partialExpr1 = choice
    [ try $ lexeme "(" *> partialExpr' <* lexeme ")"
    , Fix <$> varid'
    , Fix <$> lit'
    ]
    <?> "expression"
    where
        partialExpr' = wrapFix . P . unwrapFix <$> partialExpr
        varid' = E . VarEF <$> varid
        lit' = E . LitEF <$> lit

infixOp :: Parser Name
infixOp = symvar <|> symcon <?> "operator"

symvar :: Parser Name
symvar = T.pack <$>
    liftA2 (:) (satisfy isVarSym) (many $ satisfy isSym)

symcon :: Parser Name
symcon = T.pack <$>
    liftA2 (:) (char ':') (many $ satisfy isSym)

pat1 :: Parser Pat'
pat1 = VarP <$> lexeme varid
    <?> "pattern"

conid :: Parser ConId
conid = NameCon <$> lexeme namecon
    <|> SymCon <$> lexeme (char '(' *> symcon <* char ')')
    <?> "constructor identifier"

namecon :: Parser Name
namecon = T.pack <$>
    liftA2 (:) (satisfy isUpper)
               (many $ satisfy isNameTail)

varid :: Parser VarId
varid = NameVar <$> try (lexeme namevar)
    <|> SymVar <$> lexeme (char '(' *> symvar <* char ')')
    <?> "variable identifier"

decls :: Parser [PartialDecl']
decls = L.indentBlock scn p where
    p = do
        a <- "wtf"
        pure (L.IndentSome (Just pos1) pure decl)

t :: Parser [PartialDecl']
t = do
    space
    i <- L.indentLevel
    let indentGuard = L.indentGuard scn EQ i
    -- indentGuard *> decl *> eol *> indentGuard *> decl
    rec ds <- indentGuard *> decl <|> eof
    many $ indentGuard *> decl <* (eol <|> eof)

namevar :: Parser Name
namevar = word
        & withPredicate (`notElem` ["where"]) empty
    where word = T.pack <$>
            liftA2 (:) (satisfy isLower) (many $ satisfy isNameTail)

isNameTail :: Char -> Bool
isNameTail c = isAlphaNum c
            || c == '\''
            || c == '_'

isVarSym :: Char -> Bool
isVarSym = (`T.elem` "\\!#$%&*+./<=>?@^|-~")

isSym :: Char -> Bool
isSym c = c == ':' || isVarSym c

infixD :: Parser (Decl' e)
infixD = do
        o <- getOffset
        a <- infixWord
        p <- prec
        op <- infixOp
        region (setErrorOffset o) $ updateOpTable a p op
        pure $ InfixD a p op
    where
        infixWord :: Parser Assoc
        infixWord = choice $ lexeme <$>
            [ "infixr" $> InfixR
            , "infixl" $> InfixL
            , "infix"  $> Infix
            ]
        
        prec :: Parser Int
        prec = do
            o <- getOffset
            n <- lexeme L.decimal <?> "precedence level (an integer)"
            if 0 <= n && n <= 9 then
                pure n
            else
                region (setErrorOffset o) $
                    registerCustomFailure (RlpParErrOutOfBoundsPrecedence n)
                    $> 9

        updateOpTable :: Assoc -> Int -> Name -> Parser ()
        updateOpTable a p op = do
                t <- use psOpTable
                psOpTable <~ H.alterF f op t
            where
                f Nothing  = pure (Just (a,p))
                f (Just x) = registerCustomFailure RlpParErrDuplicateInfixD
                           $> Just x

tySigD = undefined

dataD :: Parser (Decl' e)
dataD = DataD <$> (lexeme "data" *> conid) <*> many typaram
      <*> optionalList (symbol "=" *> conalts)
    where
        typaram :: Parser Name
        typaram = lexeme namevar

        conalts :: Parser [ConAlt]
        conalts = (:) <$> conalt <*> optionalList (symbol "|" *> conalts)

        conalt :: Parser ConAlt
        conalt = ConAlt <$> conid <*> many type1

type1 :: Parser Type
type1 = choice
    [ lexeme "(" *> type_ <* lexeme ")"
    , TyVar <$> namevar
    , TyCon <$> namecon
    ]

type_ :: Parser Type
type_ = choice
    [ try $ (:->) <$> type1 <*> (lexeme "->" *> type_)
    , type1
    ]

lit :: Parser Lit'
lit = int
    <?> "literal"
    where
        int = IntL <$> L.decimal

--------------------------------------------------------------------------------
-- completing partial expressions

complete :: (?pt :: OpTable) => PartialExpr' -> RlpExpr'
complete = cata completePartial

completePartial :: (?pt :: OpTable) => PartialE -> RlpExpr'
completePartial (E e)        = completeRlpExpr e
completePartial p@(B o l r)  = completeB (build p)
completePartial (P e)        = completePartial e

completeRlpExpr :: (?pt :: OpTable) => RlpExprF' RlpExpr' -> RlpExpr'
completeRlpExpr = embed

completeB :: (?pt :: OpTable) => PartialE -> RlpExpr'
completeB p = case build p of
    B o l r     -> (o' `AppE` l') `AppE` r'
        where
            -- TODO: how do we know it's symbolic?
            o' = VarE (SymVar o)
            l' = completeB l
            r' = completeB r
    P e         -> completeB e
    E e         -> completeRlpExpr e

build :: (?pt :: OpTable) => PartialE -> PartialE
build e = go id e (rightmost e) where
    rightmost :: PartialE -> PartialE
    rightmost (B _ _ r) = rightmost r
    rightmost p@(E _)   = p
    rightmost p@(P _)   = p

    go :: (?pt :: OpTable)
       => (PartialE -> PartialE)
       -> PartialE -> PartialE -> PartialE
    go f p@(WithInfo o _ r) = case r of
            E _     -> mkHole o (f . f')
            P _     -> mkHole o (f . f')
            B _ _ _ -> go (mkHole o (f . f')) r
        where f' r' = p & pR .~ r'
    go f _ = id

mkHole :: (?pt :: OpTable)
       => OpInfo
       -> (PartialE -> PartialE)
       -> PartialE
       -> PartialE
mkHole _     hole p@(P _)                   = hole p
mkHole _     hole p@(E _)                   = hole p
mkHole (a,d) hole p@(WithInfo (a',d') _ _)
    | d' < d  = above
    | d' > d  = below
    | d == d' = case (a,a') of
                  -- left-associative operators of equal precedence are
                  -- associated left
                  (InfixL,InfixL) -> above
                  -- right-associative operators are handled similarly
                  (InfixR,InfixR) -> below
                  -- non-associative operators of equal precedence, or equal
                  -- precedence operators of different associativities are
                  -- invalid
                  (_,     _)      -> error "invalid expression"
    where
        above = p & pL %~ hole
        below = hole p

examplePrecTable :: OpTable
examplePrecTable = H.fromList
    [ ("+",    (InfixL,6))
    , ("*",    (InfixL,7))
    , ("^",    (InfixR,8))
    , (".",    (InfixR,7))
    , ("~",    (Infix, 9))
    , ("=",    (Infix, 4))
    , ("&&",   (Infix, 3))
    , ("||",   (Infix, 2))
    , ("$",    (InfixR,0))
    , ("&",    (InfixL,0))
    ]

