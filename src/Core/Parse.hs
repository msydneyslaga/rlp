{-# LANGUAGE LambdaCase, BlockArguments #-}
module Core.Parse
    ( parseCore
    , parseCoreExpr
    )
    where
----------------------------------------------------------------------------------
import Control.Parser
import Data.Functor         ((<&>), ($>))
import Core.Lex
import Core.Syntax
----------------------------------------------------------------------------------

type CoreParser = ParserT [CoreToken] Result

parseCore :: [CoreToken] -> Result Program
parseCore = fmap snd . runParserT program

parseCoreExpr :: [CoreToken] -> Result Expr
parseCoreExpr = fmap snd . runParserT expr

program :: CoreParser Program
program = Program <$> termMany (char TokSemicolon) scdef

scdef :: CoreParser ScDef
scdef = ScDef <$> f <*> (xs <* eq) <*> body
    where
        f = name
        xs = many name
        eq = char TokEquals
        body = expr

expr :: CoreParser Expr
expr = letE
   <|> app
   <|> lam
   <|> atom

atom :: CoreParser Expr
atom  = var
    <|> con
    <|> parenE
    <|> lit
    where
        var = Var <$> name
        parenE = surround (char TokLParen) (char TokRParen) expr
        lit = IntE <$> litInt

lam :: CoreParser Expr
lam = Lam <$> (l *> bs) <*> (arrow *> expr)
    where
        l = char TokLambda
        arrow = char TokArrow
        bs = some name

app :: CoreParser Expr
app = foldl App <$> atom <*> some atom

con :: CoreParser Expr
con = pack *> (Con <$> (l *> tag) <*> (arity <* r))
    where
        l = char TokLBrace
        r = char TokRBrace
        tag = litInt
        arity = litInt
        pack = match \case
            TokCName "Pack" -> Just ()
            _               -> Nothing

letE :: CoreParser Expr
letE = Let <$> word <*> defs <*> (char TokIn *> expr)
    where
        word = char TokLet    $> NonRec
           <|> char TokLetRec $> Rec
        defs = surround (char TokLBrace) (char TokRBrace) bindings

bindings :: CoreParser [Binding]
bindings = sepSome (char TokSemicolon) binding

binding :: CoreParser Binding
binding = Binding <$> name <*> (char TokEquals *> expr)

----------------------------------------------------------------------------------

name :: CoreParser Name
name = match \case
    TokName n -> Just n
    _         -> Nothing

cName :: CoreParser Name
cName = match \case
    TokCName n -> Just n
    _          -> Nothing

litInt :: CoreParser Int
litInt = match \case
    TokLitInt n -> Just n
    _           -> Nothing

