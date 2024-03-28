{
module Rlp.AltParse
    ( parseRlpProg
    , parseRlpProgR
    , parseRlpExprR
    , runP'
    )
    where
import Data.List.Extra
import Data.Text                (Text)

import Control.Comonad
import Control.Comonad.Cofree
import Control.Lens             hiding (snoc)

import Compiler.RlpcError
import Compiler.RLPC
import Control.Monad.Errorful

import Rlp.Lex
import Rlp.AltSyntax
import Rlp.Parse.Types          hiding (PsName)
import Core.Syntax              qualified as Core
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
    'λ'             { Located _ TokenLambda }
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
    forall          { Located _ TokenForall }

%nonassoc '='
%right '->'
%right in

%%

StandaloneProgram :: { Program Name (RlpExpr PsName) }
                  : layout0(Decl)       { Program $1 }


StandaloneExpr :: { RlpExpr PsName }
               : VL Expr VR             { $2 }

VL  :: { () }
VL  : vlbrace       { () }

VR  :: { () }
VR  : vrbrace       { () }
    | error         { () }

VS                  :: { () }
VS                  : ';'                   { () }
                    | vsemi                 { () }

Decl                :: { Decl PsName (RlpExpr PsName) }
                    : FunD                  { $1 }
                    | DataD                 { $1 }
                    | TySigD                { $1 }

TySigD              :: { Decl PsName (RlpExpr PsName) }
                    : Var '::' Type         { TySigD $1 $3 }

DataD               :: { Decl PsName (RlpExpr PsName) }
                    : data Con TyVars               { DataD $2 $3 [] }
                    | data Con TyVars '=' DataCons  { DataD $2 $3 $5 }

DataCons            :: { [DataCon PsName] }
                    : DataCon '|' DataCons  { $1 : $3 }
                    | DataCon               { [$1] }

DataCon             :: { DataCon PsName }
                    : Con list0(Type1)      { DataCon $1 $2 }

Type1               :: { Type PsName }
                    : varname               { VarT $ extractVarName $1 }
                    | Con                   { ConT $1 }
                    | '(' Type ')'          { $2 }

Type                :: { Type PsName }
                    : Type '->' Type        { $1 :-> $3 }
                    | AppT                  { $1 }

AppT                :: { Type PsName }
                    : Type1                 { $1 }
                    | AppT Type1            { AppT $1 $2 }

TyVars              :: { [PsName] }
                    : list0(varname)     { $1 <&> view ( to extract
                                                       . singular _TokenVarName ) }

FunD                :: { Decl PsName (RlpExpr PsName) }
                    : Var Pat1s '=' Expr    { FunD $1 $2 $4 }

Expr                :: { RlpExpr PsName }
                    : AppE                  { $1 }
                    | LetE                  { $1 }
                    | CaseE                 { $1 }
                    | LamE                  { $1 }

LamE                :: { RlpExpr PsName }
                    : 'λ' list0(varname) '->' Expr { Finl $ Core.LamF (fmap extractName $2) $4 }

CaseE               :: { RlpExpr PsName }
                    : case Expr of CaseAlts { Finr $ CaseEF $2 $4 }

CaseAlts            :: { [Alter PsName (RlpExpr PsName)] }
                    : layout1(CaseAlt)      { $1 }

CaseAlt             :: { Alter PsName (RlpExpr PsName) }
                    : Pat '->' Expr    { Alter $1 $3 }

LetE                :: { RlpExpr PsName }
                    : let layout1(Binding) in Expr
                        { Finr $ LetEF Core.NonRec $2 $4 }
                    | letrec layout1(Binding) in Expr
                        { Finr $ LetEF Core.Rec $2 $4 }

Binding             :: { Binding PsName (RlpExpr PsName) }
                    : Pat '=' Expr          { VarB $1 $3 }

Expr1               :: { RlpExpr PsName }
                    : VarE                  { $1 }
                    | litint                { $1 ^. to extract
                                                  . singular _TokenLitInt
                                                  . to (Finl . Core.LitF . Core.IntL) }
                    | '(' Expr ')'          { $2 }
                    | ConE                  { $1 }

AppE                :: { RlpExpr PsName }
                    : AppE Expr1            { Finl $ Core.AppF $1 $2 }
                    | Expr1                 { $1 }

VarE                :: { RlpExpr PsName }
                    : Var                   { Finl $ Core.VarF $1 }

ConE                :: { RlpExpr PsName }
                    : Con                   { Finl $ Core.VarF $1 }

Pat1s               :: { [Pat PsName] }
                    : list0(Pat1)           { $1 }

Pat1                :: { Pat PsName }
                    : Var                   { VarP $1 }
                    | Con                   { ConP $1 }
                    | '(' Pat ')'           { $2 }

Pat                 :: { Pat PsName }
                    : AppP                  { $1 }

AppP                :: { Pat PsName }
                    : Pat1                  { $1 }
                    | AppP Pat1             { $1 `AppP` $2 }

Con                 :: { PsName }
                    : conname               { $1 ^. to extract
                                                  . singular _TokenConName }
                    | '(' consym ')'        { $1 ^. to extract
                                                  . singular _TokenConSym }

Var                 :: { PsName }
                    : varname               { $1 ^. to extract
                                                  . singular _TokenVarName }
                    | '(' varsym ')'        { $2 ^. to extract
                                                  . singular _TokenVarSym }

-- list0(p : α) : [α]
list0(p) : {- epsilon -}            { [] }
         | list0(p) p             { $1 `snoc` $2 }

-- layout0(p : β) :: [β]
layout0(p)  : '{' '}'                   { [] }
            | VL  VR                    { [] }
            | layout1(p)                { $1 }

-- layout_list0(sep : α, p : β) :: [β]
layout_list0(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }
                    | {- epsilon -}              { [] }

-- layout1(p : β) :: [β]
layout1(p)  : '{' layout_list1(';',p) '}'   { $2 }
            | VL  layout_list1(VS,p) VS VR  { $2 }
            | VL  layout_list1(VS,p) VR     { $2 }

-- layout_list1(sep : α, p : β) :: [β]
layout_list1(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }

{

extractVarName = view $ to extract . singular _TokenVarName

parseRlpProgR :: (Monad m) => Text -> RLPCT m (Program Name (RlpExpr PsName))
parseRlpProgR s = liftErrorful $ errorful (ma,es)
    where
        (_,es,ma) = runP' parseRlpProg s

parseRlpExprR :: (Monad m) => Text -> RLPCT m (RlpExpr PsName)
parseRlpExprR s = liftErrorful $ errorful (ma,es)
    where
        (_,es,ma) = runP' parseRlpExpr s

parseError :: (Located RlpToken, [String]) -> P a
parseError (Located ss t,ts) = addFatalHere (ss ^. srcSpanLen) $
    RlpParErrUnexpectedToken t ts

extractName = view $ to extract . singular _TokenVarName

}
