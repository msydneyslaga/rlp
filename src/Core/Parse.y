{
{-|
Module : Core.Parse
Description : Parser for the Core language
-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Parse
    ( parseCore
    , parseCoreExpr
    , parseCoreProg
    , parseCoreProgR
    , module Core.Lex -- temp convenience
    , parseTmp
    , SrcError
    , Module
    )
    where

import Control.Monad        ((>=>))
import Data.Foldable        (foldl')
import Core.Syntax
import Core.Lex
import Compiler.RLPC
import Lens.Micro
import Data.Default.Class   (def)
import Data.Hashable        (Hashable)
import Data.Text.IO         qualified as TIO
import Data.Text            qualified as T
import Data.HashMap.Strict  qualified as H
}

%name parseCore Module
%name parseCoreExpr StandaloneExpr
%name parseCoreProg StandaloneProgram
%tokentype { Located CoreToken }
%error { parseError }
%monad { RLPC SrcError }

%token
      let             { Located _ _ _ TokenLet }
      letrec          { Located _ _ _ TokenLetrec }
      module          { Located _ _ _ TokenModule }
      where           { Located _ _ _ TokenWhere }
      case            { Located _ _ _ TokenCase }
      of              { Located _ _ _ TokenOf }
      pack            { Located _ _ _ TokenPack } -- temp
      in              { Located _ _ _ TokenIn }
      litint          { Located _ _ _ (TokenLitInt $$) }
      varname         { Located _ _ _ (TokenVarName $$) }
      varsym          { Located _ _ _ (TokenVarSym $$) }
      conname         { Located _ _ _ (TokenConName $$) }
      consym          { Located _ _ _ (TokenConSym $$) }
      word            { Located _ _ _ (TokenWord $$) }
      'λ'             { Located _ _ _ TokenLambda }
      '->'            { Located _ _ _ TokenArrow }
      '='             { Located _ _ _ TokenEquals }
      '@'             { Located _ _ _ TokenTypeApp }
      '('             { Located _ _ _ TokenLParen }
      ')'             { Located _ _ _ TokenRParen }
      '{'             { Located _ _ _ TokenLBrace }
      '}'             { Located _ _ _ TokenRBrace }
      '{-#'           { Located _ _ _ TokenLPragma }
      '#-}'           { Located _ _ _ TokenRPragma }
      ';'             { Located _ _ _ TokenSemicolon }
      '::'            { Located _ _ _ TokenHasType }
      eof             { Located _ _ _ TokenEOF }

%%

Module          :: { Module Name }
Module          : module conname where Program Eof { Module (Just ($2, [])) $4 }
                | Program Eof                      { Module Nothing $1 }

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

StandaloneProgram :: { Program Name }
StandaloneProgram : Program eof                 { $1 }

Program         :: { Program Name }
Program         : ScTypeSig ';' Program         { insTypeSig $1 $3 }
                | ScTypeSig OptSemi             { singletonTypeSig $1 }
                | ScDef     ';' Program         { insScDef $1 $3 }
                | ScDef     OptSemi             { singletonScDef $1 }

OptSemi         :: { () }
OptSemi         : ';'                           { () }
                | {- epsilon -}                 { () }

ScTypeSig       :: { (Name, Type) }
ScTypeSig       : Var '::' Type                 { ($1,$3) }

ScDefs          :: { [ScDef Name] }
ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
                | ScDef ';'                     { [$1] }
                | ScDef                         { [$1] }
                | {- epsilon -}                 { [] }

ScDef           :: { ScDef Name }
ScDef           : Var ParList '=' Expr          { ScDef $1 $2 $4 }

Type            :: { Type }
Type            : Type1                         { $1 }

Type1           :: { Type }
Type1           : '(' Type ')'                  { $2 }
                | Type1 '->' Type               { $1 :-> $3 }
                -- do we want to allow symbolic names for tyvars and tycons?
                | varname                       { TyVar $1 }
                | conname                       { TyCon $1 }

ParList         :: { [Name] }
ParList         : Var ParList                   { $1 : $2 }
                | {- epsilon -}                 { [] }

StandaloneExpr  :: { Expr Name }
StandaloneExpr  : Expr eof                      { $1 }

Expr            :: { Expr Name }
Expr            : LetExpr                       { $1 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | CaseExpr                      { $1 }
                | Expr1                         { $1 }

LetExpr         :: { Expr Name }
LetExpr         : let    '{'   Bindings '}'    in Expr { Let NonRec $3 $6 }
                | letrec '{'   Bindings '}'    in Expr { Let Rec $3 $6 }

Binders         :: { [Name] }
Binders         : Var Binders                  { $1 : $2 }
                | Var                          { [$1] }

Application     :: { Expr Name }
Application     : Expr1 AppArgs                { foldl' App $1 $2 }

AppArgs         :: { [Expr Name] }
AppArgs         : Expr1 AppArgs                 { $1 : $2 }
                | Expr1                         { [$1] }

CaseExpr        :: { Expr Name }
CaseExpr        : case Expr of '{' Alters '}'   { Case $2 $5 }

Alters          :: { [Alter Name] }
Alters          : Alter ';' Alters              { $1 : $3 }
                | Alter ';'                     { [$1] }
                | Alter                         { [$1] }

Alter           :: { Alter Name }
Alter           : litint ParList '->' Expr      { Alter (AltData $1) $2 $4 }

Expr1           :: { Expr Name }
Expr1           : litint                        { Lit $ IntL $1 }
                | Id                            { Var $1 }
                | PackCon                       { $1 }
                | ExprPragma                    { $1 }
                | '(' Expr ')'                  { $2 }

ExprPragma      :: { Expr Name }
ExprPragma      : '{-#' Words '#-}'      {% exprPragma $2 }

Words           :: { [String] }
Words           : word Words                    { T.unpack $1 : $2 }
                | word                          { [T.unpack $1] }

PackCon         :: { Expr Name }
PackCon         : pack '{' litint litint '}'    { Con $3 $4 }

Bindings        :: { [Binding Name] }
Bindings        : Binding ';' Bindings          { $1 : $3 }
                | Binding ';'                   { [$1] }
                | Binding                       { [$1] }

Binding         :: { Binding Name }
Binding         : Var '=' Expr                  { $1 := $3 }

Id              :: { Name }
Id              : Var                                { $1 }
                | Con                                { $1 }

Var             :: { Name }
Var             : '(' varsym ')'                    { $2 }
                | varname                           { $1 }

Con             :: { Name }
Con             : '(' consym ')'                    { $2 }
                | conname                           { $1 }

{

parseError :: [Located CoreToken] -> RLPC SrcError a
parseError (Located y x l _ : _) = addFatal err
    where err = SrcError
            { _errSpan       = (y,x,l)
            , _errSeverity   = Error
            , _errDiagnostic = SrcErrParse
            }

parseTmp :: IO (Module Name)
parseTmp = do
    s <- TIO.readFile "/tmp/t.hs"
    case parse s of
        Left e -> error (show e)
        Right (ts,_) -> pure ts
    where
        parse = evalRLPC def . (lexCore >=> parseCore)

exprPragma :: [String] -> RLPC SrcError (Expr Name)
exprPragma ("AST" : e) = astPragma e
exprPragma _           = addFatal err
    where err = SrcError
            { _errSpan       = (0,0,0) -- TODO: span
            , _errSeverity   = Warning
            , _errDiagnostic = SrcErrUnknownPragma "" -- TODO: missing pragma
            }

astPragma :: [String] -> RLPC SrcError (Expr Name)
astPragma = pure . read . unwords

insTypeSig :: (Hashable b) => (b, Type) -> Program b -> Program b
insTypeSig ts = programTypeSigs %~ uncurry H.insert ts

singletonTypeSig :: (Hashable b) => (b, Type) -> Program b
singletonTypeSig ts = insTypeSig ts mempty

insScDef :: (Hashable b) => ScDef b -> Program b -> Program b
insScDef sc = programScDefs %~ (sc:)

singletonScDef :: (Hashable b) => ScDef b -> Program b
singletonScDef sc = insScDef sc mempty

parseCoreProgR :: [Located CoreToken] -> RLPC RlpcError Program'
parseCoreProgR = liftRlpcErrs . parseCoreProg

}

