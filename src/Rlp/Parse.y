{
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Rlp.Parse
    ( parseRlpProg
    , parseRlpProgR
    , parseRlpExpr
    , parseRlpExprR
    )
    where
import Compiler.RlpcError
import Compiler.RLPC
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
import Rlp.Parse.Associate
import Control.Lens                 hiding (snoc, (.>), (<.), (<<~))
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

StandaloneProgram   :: { Program RlpcPs }
StandaloneProgram   : '{' Decls  '}'        { undefined }
                    | VL  DeclsV VR         { undefined }

StandaloneExpr      :: { Expr RlpcPs }
                    : VL Expr VR            { undefined }

VL  :: { () }
VL  : vlbrace       { undefined }

VR  :: { () }
VR  : vrbrace       { undefined }
    | error         { undefined }

Decls               :: { [Decl RlpcPs] }
Decls               : Decl ';' Decls        { undefined }
                    | Decl ';'              { undefined }
                    | Decl                  { undefined }

DeclsV              :: { [Decl RlpcPs] }
DeclsV              : Decl VS DeclsV        { undefined }
                    | Decl VS               { undefined }
                    | Decl                  { undefined }

VS                  :: { Located RlpToken }
VS                  : ';'                   { undefined }
                    | vsemi                 { undefined }

Decl        :: { Decl RlpcPs }
            : FunDecl                   { undefined }
            | TySigDecl                 { undefined }
            | DataDecl                  { undefined }
            | InfixDecl                 { undefined }

TySigDecl   :: { Decl RlpcPs }
            : Var '::' Type             { undefined }

InfixDecl   :: { Decl RlpcPs }
            : InfixWord litint InfixOp  { undefined }

InfixWord   :: { Located Assoc }
            : infixl                    { undefined }
            | infixr                    { undefined }
            | infix                     { undefined }

DataDecl    :: { Decl RlpcPs }
            : data Con TyParams '=' DataCons    { undefined }

TyParams    :: { [PsName] }
            : {- epsilon -}             { undefined }
            | TyParams varname          { undefined }

DataCons    :: { [ConAlt RlpcPs] }
            : DataCons '|' DataCon      { undefined }
            | DataCon                   { undefined }

DataCon     :: { ConAlt RlpcPs }
            : Con Type1s                { undefined }

Type1s      :: { [Ty RlpcPs] }
            : {- epsilon -}             { undefined }
            | Type1s Type1              { undefined }

Type1       :: { Ty RlpcPs }
            : '(' Type ')'              { undefined }
            | conname                   { undefined }
            | varname                   { undefined }

Type        :: { Ty RlpcPs }
            : Type '->' Type            { undefined }
            | TypeApp                   { undefined }

TypeApp     :: { Ty RlpcPs }
            : Type1                     { undefined }
            | TypeApp Type1             { undefined }

FunDecl     :: { Decl RlpcPs }
FunDecl     : Var Params '=' Expr       { undefined }

Params      :: { [Pat RlpcPs] }
Params      : {- epsilon -}             { undefined }
            | Params Pat1               { undefined }

Pat         :: { Pat RlpcPs }
            : Con Pat1s                 { undefined }
            | Pat1                      { undefined }

Pat1s       :: { [Pat RlpcPs] }
            : Pat1s Pat1                { undefined }
            | Pat1                      { undefined }

Pat1        :: { Pat RlpcPs }
            : Con                       { undefined }
            | Var                       { undefined }
            | Lit                       { undefined }
            | '(' Pat ')'               { undefined }

Expr        :: { Expr RlpcPs }
            -- infixities delayed till next release :(
            -- : Expr1 InfixOp Expr        { undefined }
            : TempInfixExpr             { undefined }
            | LetExpr                   { undefined }
            | CaseExpr                  { undefined }
            | AppExpr                   { undefined }

TempInfixExpr :: { Expr RlpcPs }
TempInfixExpr : Expr1 InfixOp TempInfixExpr { undefined }
              | Expr1 InfixOp Expr1 { undefined }

AppExpr     :: { Expr RlpcPs }
            : Expr1                     { undefined }
            | AppExpr Expr1             { undefined }

LetExpr     :: { Expr RlpcPs }
            : let layout1(Binding) in Expr      { undefined }
            | letrec layout1(Binding) in Expr   { undefined }

CaseExpr    :: { Expr RlpcPs }
            : case Expr of layout0(CaseAlt)     { undefined }

-- TODO: where-binds
CaseAlt     :: { (Alt RlpcPs, Where RlpcPs) }
            : Alt                           { undefined }

Alt         :: { Alt RlpcPs }
            : Pat '->' Expr                 { undefined }

-- layout0(p : β) :: [β]
layout0(p)  : '{' layout_list0(';',p) '}'   { undefined }
            | VL  layout_list0(VS,p)  VR    { undefined }

-- layout_list0(sep : α, p : β) :: [β]
layout_list0(sep,p) : p                          { undefined }
                    | layout_list1(sep,p) sep p  { undefined }
                    | {- epsilon -}              { undefined }

-- layout1(p : β) :: [β]
layout1(p)  : '{' layout_list1(';',p) '}'   { undefined }
            | VL  layout_list1(VS,p) VR     { undefined }

-- layout_list1(sep : α, p : β) :: [β]
layout_list1(sep,p) : p                          { undefined }
                    | layout_list1(sep,p) sep p  { undefined }

Binding     :: { Binding RlpcPs }
            : Pat '=' Expr              { undefined }

Expr1       :: { Expr RlpcPs }
            : '(' Expr ')'              { undefined }
            | Lit                       { undefined }
            | Var                       { undefined }
            | Con                       { undefined }

InfixOp     :: { Located PsName }
            : consym                    { undefined }
            | varsym                    { undefined }

-- TODO: microlens-pro save me microlens-pro (rewrite this with prisms)
Lit         :: { Lit RlpcPs }
            : litint                    { undefined }

Var         :: { Located PsName }
Var         : varname                   { undefined }
            | varsym                    { undefined }

Con         :: { Located PsName }
            : conname                   { undefined }

--}

{

parseRlpProgR = undefined
parseRlpExprR = undefined
    
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

mkProgram :: [Decl RlpcPs] -> P (Program RlpcPs)
mkProgram ds = do
    pt <- use psOpTable
    pure $ Program (associate pt <$> ds)

mkInfixD :: Assoc -> Int -> PsName -> P (Decl RlpcPs)
mkInfixD a p n = do
    let opl :: Lens' ParseState (Maybe OpInfo)
        opl = psOpTable . at n
    opl <~ (use opl >>= \case
        Just o  -> addWoundHere l e >> pure (Just o) where
            e = RlpParErrDuplicateInfixD n
            l = T.length n
        Nothing -> pure (Just (a,p))
        )
    pos <- use (psInput . aiPos)
    pure $ Located (spanFromPos pos 0) (InfixD a p n)

intOfToken :: Located RlpToken -> Int
intOfToken (Located _ (TokenLitInt n)) = n

tempInfixExprErr :: Expr RlpcPs -> Expr RlpcPs -> P a
tempInfixExprErr (Located a _) (Located b _) =
    addFatal $ errorMsg (a <> b) $ RlpParErrOther
        [ "The rl' frontend is currently in beta. Support for infix expressions is minimal, sorry! :("
        , "In the mean time, don't mix any infix operators."
        ]

--}

mkPsName = undefined
tempInfixExprErr = undefined
extractName = undefined
extractInt = undefined
mkProgram = undefined

parseError :: (Located RlpToken, [String]) -> P a
parseError ((Located ss t), exp) = addFatal $
    errorMsg ss (RlpParErrUnexpectedToken t exp)

}

