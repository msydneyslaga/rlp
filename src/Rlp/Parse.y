{
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Rlp.Parse
    ( parseRlpProg
    , parseRlpProgR
    , parseRlpExpr
    , parseRlpExprR
    , runP'
    )
    where
import Compiler.RlpcError
import Compiler.RLPC
import Control.Comonad.Cofree
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
import Rlp.Parse.Associate
import Control.Lens                 hiding (snoc, (.>), (<.), (<<~), (:<))
import Data.List.Extra
import Data.Fix
import Data.Functor.Const
import Data.Functor.Apply
import Data.Functor.Bind
import Control.Comonad
import Data.Functor
import Data.Semigroup.Traversable
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.Void
import Compiler.Types
}

%name parseRlpProg StandaloneProgram
%name parseRlpExpr StandaloneExpr

%monad { P }
%lexer { lexCont } { Located _ TokenEOF }
%error { parseError }
%errorhandlertype explist
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName _) }
    conname         { Located _ (TokenConName _) }
    consym          { Located _ (TokenConSym _) }
    varsym          { Located _ (TokenVarSym _) }
    data            { Located _ TokenData }
    case            { Located _ TokenCase }
    of              { Located _ TokenOf }
    litint          { Located _ (TokenLitInt _) }
    '='             { Located _ TokenEquals }
    '|'             { Located _ TokenPipe }
    '::'            { Located _ TokenHasType }
    ';'             { Located _ TokenSemicolon }
    '('             { Located _ TokenLParen }
    ')'             { Located _ TokenRParen }
    '->'            { Located _ TokenArrow }
    vsemi           { Located _ TokenSemicolonV }
    '{'             { Located _ TokenLBrace }
    '}'             { Located _ TokenRBrace }
    vlbrace         { Located _ TokenLBraceV }
    vrbrace         { Located _ TokenRBraceV }
    infixl          { Located _ TokenInfixL }
    infixr          { Located _ TokenInfixR }
    infix           { Located _ TokenInfix }
    let             { Located _ TokenLet }
    letrec          { Located _ TokenLetrec }
    in              { Located _ TokenIn }

%nonassoc '='
%right '->'
%right in

%%

StandaloneProgram   :: { Program RlpcPs SrcSpan }
StandaloneProgram   : layout0(Decl)         {% mkProgram $1 }

StandaloneExpr      :: { Expr' RlpcPs SrcSpan }
                    : VL Expr VR            { $2 }

VL  :: { () }
VL  : vlbrace       { () }

VR  :: { () }
VR  : vrbrace       { () }
    | error         {% void popLayout }

VS                  :: { () }
VS                  : ';'                   { () }
                    | vsemi                 { () }

Decl        :: { Decl RlpcPs SrcSpan }
            : FunDecl                   { $1 }
            | TySigDecl                 { $1 }
            | DataDecl                  { $1 }
            | InfixDecl                 { $1 }

TySigDecl   :: { Decl RlpcPs SrcSpan }
            : Var '::' Type             { TySigD [$1] $3 }

InfixDecl   :: { Decl RlpcPs SrcSpan }
            : InfixWord litint InfixOp  {% mkInfixD $1 ($2 ^. _litint) $3 }

InfixWord   :: { Assoc }
            : infixl                    { InfixL }
            | infixr                    { InfixR }
            | infix                     { Infix }

DataDecl    :: { Decl RlpcPs SrcSpan }
            : data Con TyParams '=' DataCons    { DataD $2 $3 $5 }

TyParams    :: { [PsName] }
            : {- epsilon -}             { [] }
            | TyParams varname          { $1 `snoc` extractName $2 }

DataCons    :: { [ConAlt RlpcPs] }
            : DataCons '|' DataCon      { $1 `snoc` $3 }
            | DataCon                   { [$1] }

DataCon     :: { ConAlt RlpcPs }
            : Con Type1s                { ConAlt $1 $2 }

Type1s      :: { [Ty RlpcPs] }
            : {- epsilon -}             { [] }
            | Type1s Type1              { $1 `snoc` $2 }

Type1       :: { Ty RlpcPs }
            : '(' Type ')'              { $2 }
            | conname                   { ConT (extractName $1) }
            | varname                   { VarT (extractName $1) }

Type        :: { Ty RlpcPs }
            : Type '->' Type            { FunT $1 $3 }
            | TypeApp                   { $1 }

TypeApp     :: { Ty RlpcPs }
            : Type1                     { $1 }
            | TypeApp Type1             { AppT $1 $2 }

FunDecl     :: { Decl RlpcPs SrcSpan }
FunDecl     : Var Params '=' Expr       { FunD $1 $2 $4 Nothing }

Params      :: { [Pat RlpcPs] }
Params      : {- epsilon -}             { [] }
            | Params Pat1               { $1 `snoc` $2 }

Pat         :: { Pat RlpcPs }
            : Con Pat1s                 { ConP $1 $2 }
            | Pat1                      { $1 }

Pat1s       :: { [Pat RlpcPs] }
            : Pat1s Pat1                { $1 `snoc` $2 }
            | Pat1                      { [$1] }

Pat1        :: { Pat RlpcPs }
            : Con                       { ConP $1 [] }
            | Var                       { VarP $1 }
            | Lit                       { LitP $1 }
            | '(' Pat ')'               { $2 }

Expr        :: { Expr' RlpcPs SrcSpan }
            -- infixities delayed till next release :(
            -- : Expr1 InfixOp Expr        { undefined }
            : AppExpr                   { $1 }
            | TempInfixExpr             { $1 }
            | LetExpr                   { $1 }
            | CaseExpr                  { $1 }

TempInfixExpr :: { Expr' RlpcPs SrcSpan }
TempInfixExpr : Expr1 InfixOp TempInfixExpr {% tempInfixExprErr $1 $3 }
              | Expr1 InfixOp Expr1         { nolo' $ InfixEF $2 $1 $3 }

AppExpr     :: { Expr' RlpcPs SrcSpan }
            : Expr1                     { $1 }
            | AppExpr Expr1             { comb2 AppEF $1 $2 }

LetExpr     :: { Expr' RlpcPs SrcSpan }
            : let layout1(Binding) in Expr      { nolo' $ LetEF NonRec $2 $4 }
            | letrec layout1(Binding) in Expr   { nolo' $ LetEF Rec $2 $4 }

CaseExpr    :: { Expr' RlpcPs SrcSpan }
            : case Expr of layout0(Alt)     { nolo' $ CaseEF $2 $4 }

-- TODO: where-binds
Alt         :: { Alt' RlpcPs SrcSpan }
            : Pat '->' Expr                 { AltA $1 (view _unwrap $3) Nothing }

-- layout0(p : β) :: [β]
layout0(p)  : '{' layout_list0(';',p) '}'   { $2 }
            | VL  layout_list0(VS,p)  VR    { $2 }

-- layout_list0(sep : α, p : β) :: [β]
layout_list0(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }
                    | {- epsilon -}              { [] }

-- layout1(p : β) :: [β]
layout1(p)  : '{' layout_list1(';',p) '}'   { $2 }
            | VL  layout_list1(VS,p) VR     { $2 }

-- layout_list1(sep : α, p : β) :: [β]
layout_list1(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }

Binding     :: { Binding' RlpcPs SrcSpan }
            : Pat '=' Expr              { PatB $1 (view _unwrap $3) }

Expr1       :: { Expr' RlpcPs SrcSpan }
            : '(' Expr ')'              { $2 }
            | Lit                       { nolo' $ LitEF $1 }
            | Var                       { case $1 of Located ss _ -> ss :< VarEF $1 }
            | Con                       { case $1 of Located ss _ -> ss :< VarEF $1 }

InfixOp     :: { PsName }
            : consym                    { extractName $1 }
            | varsym                    { extractName $1 }

-- TODO: microlens-pro save me microlens-pro (rewrite this with prisms)
Lit         :: { Lit RlpcPs }
            : litint                    { $1 ^. to extract
                                              . singular _TokenLitInt
                                              . to IntL }

Var         :: { PsName }
Var         : varname                   { $1 <&> view (singular _TokenVarName) }
            | varsym                    { $1 <&> view (singular _TokenVarSym) }

Con         :: { PsName }
            : conname                   { $1 <&> view (singular _TokenConName) }

{

parseRlpProgR :: (Monad m) => Text -> RLPCT m (Program RlpcPs SrcSpan)
parseRlpProgR s = do
    a <- liftErrorful $ pToErrorful parseRlpProg st
    addDebugMsg @_ @String "dump-parsed" $ show a
    pure a
  where
    st = programInitState s

parseRlpExprR :: (Monad m) => Text -> RLPCT m (Expr' RlpcPs SrcSpan)
parseRlpExprR s = liftErrorful $ pToErrorful parseRlpExpr st
    where
        st = programInitState s

mkInfixD :: Assoc -> Int -> PsName -> P (Decl RlpcPs SrcSpan)
mkInfixD a p ln@(Located ss n) = do
    let opl :: Lens' ParseState (Maybe OpInfo)
        opl = psOpTable . at n
    opl <~ (use opl >>= \case
        Just o  -> addWoundHere l e >> pure (Just o) where
            e = RlpParErrDuplicateInfixD n
            l = T.length n
        Nothing -> pure (Just (a,p))
        )
    pos <- use (psInput . aiPos)
    pure $ InfixD a p ln

{--

parseRlpExprR :: (Monad m) => Text -> RLPCT m (Expr RlpcPs)
parseRlpExprR s = liftErrorful $ pToErrorful parseRlpExpr st
    where
        st = programInitState s

parseRlpProgR :: (Monad m) => Text -> RLPCT m (Program RlpcPs)
parseRlpProgR s = do
    a <- liftErrorful $ pToErrorful parseRlpProg st
    addDebugMsg @_ @String "dump-parsed" $ show a
    pure a
  where
        st = programInitState s

mkPsName :: Located RlpToken -> Located PsName
mkPsName = fmap extractName

extractName :: RlpToken -> PsName
extractName = \case
    TokenVarName n -> n
    TokenConName n -> n
    TokenConSym  n -> n
    TokenVarSym  n -> n
    _              -> error "mkPsName: not an identifier"

extractInt :: RlpToken -> Int
extractInt (TokenLitInt n) = n
extractInt _ = error "extractInt: ugh"

mkProgram :: [Decl RlpcPs SrcSpan] -> P (Program RlpcPs SrcSpan)
mkProgram ds = do
    pt <- use psOpTable
    pure $ Program (associate pt <$> ds)

intOfToken :: Located RlpToken -> Int
intOfToken (Located _ (TokenLitInt n)) = n

tempInfixExprErr :: Expr RlpcPs -> Expr RlpcPs -> P a
tempInfixExprErr (Located a _) (Located b _) =
    addFatal $ errorMsg (a <> b) $ RlpParErrOther
        [ "The rl' frontend is currently in beta. Support for infix expressions is minimal, sorry! :("
        , "In the mean time, don't mix any infix operators."
        ]

--}

_litint :: Getter (Located RlpToken) Int
_litint = to extract
        . singular _TokenLitInt

tempInfixExprErr :: Expr' RlpcPs SrcSpan -> Expr' RlpcPs SrcSpan -> P a
tempInfixExprErr (a :< _) (b :< _) =
    addFatal $ errorMsg (a <> b) $ RlpParErrOther
        [ "The rl' frontend is currently in beta. Support for infix expressions is minimal, sorry! :("
        , "In the mean time, don't mix any infix operators."
        ]

mkProgram :: [Decl RlpcPs SrcSpan] -> P (Program RlpcPs SrcSpan)
mkProgram ds = do
    pt <- use psOpTable
    pure $ Program (associate pt <$> ds)

extractName :: Located RlpToken -> PsName
extractName (Located ss (TokenVarSym n)) = Located ss n
extractName (Located ss (TokenVarName n)) = Located ss n
extractName (Located ss (TokenConName n)) = Located ss n
extractName (Located ss (TokenConSym n)) = Located ss n

parseError :: (Located RlpToken, [String]) -> P a
parseError ((Located ss t), exp) = addFatal $
    errorMsg ss (RlpParErrUnexpectedToken t exp)

}

