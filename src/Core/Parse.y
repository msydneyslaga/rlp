{
module Core.Parse
    ( parseCore
    , parseCoreExpr
    , parseCoreProg
    , module Core.Lex -- temp convenience
    , parseTmp
    , SrcError
    , ParseError
    , Module
    )
    where

import Control.Monad        ((>=>))
import Data.Foldable        (foldl')
import Core.Syntax
import Core.Lex
import Compiler.RLPC
}

%name parseCore Module
%name parseCoreExpr StandaloneExpr
%name parseCoreProg StandaloneProgram
%tokentype { Located CoreToken }
%error { parseError }
%monad { RLPC ParseError }

%token
      let             { Located _ _ _ TokenLet }
      letrec          { Located _ _ _ TokenLetrec }
      module          { Located _ _ _ TokenModule }
      where           { Located _ _ _ TokenWhere }
      ','             { Located _ _ _ TokenComma }
      in              { Located _ _ _ TokenIn }
      litint          { Located _ _ _ (TokenLitInt $$) }
      varname         { Located _ _ _ (TokenVarName $$) }
      varsym          { Located _ _ _ (TokenVarSym $$) }
      conname         { Located _ _ _ (TokenConName $$) }
      consym          { Located _ _ _ (TokenConSym $$) }
      'λ'             { Located _ _ _ TokenLambda }
      '->'            { Located _ _ _ TokenArrow }
      '='             { Located _ _ _ TokenEquals }
      '('             { Located _ _ _ TokenLParen }
      ')'             { Located _ _ _ TokenRParen }
      '{'             { Located _ _ _ TokenLBrace }
      '}'             { Located _ _ _ TokenRBrace }
      vl              { Located _ _ _ TokenLBraceV }
      vr              { Located _ _ _ TokenRBraceV }
      ';'             { Located _ _ _ TokenSemicolon }
      eof             { Located _ _ _ TokenEOF }

%%

Module          :: { Module }
Module          : module conname where Program Eof { Module (Just ($2, [])) $4 }
                | Program Eof                      { Module Nothing $1 }

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

StandaloneProgram :: { Program }
StandaloneProgram : Program eof                 { $1 }

Program         :: { Program }
Program         : VOpen ScDefs VClose           { Program $2 }
                | '{'   ScDefs '}'              { Program $2 }

VOpen           :: { () }
VOpen           : vl                            { () }

VClose          :: { () }
VClose          : vr                            { () }
                | error                         { () }

ScDefs          :: { [ScDef] }
ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
                | {- epsilon -}                 { [] }

ScDef           :: { ScDef }
ScDef           : Var ParList '=' Expr          { ScDef $1 $2 $4 }

ParList         :: { [Name] }
ParList         : Var ParList                   { $1 : $2 }
                | {- epsilon -}                 { [] }

StandaloneExpr  :: { Expr }
StandaloneExpr  : Expr eof                      { $1 }

Expr            :: { Expr }
Expr            : LetExpr                       { $1 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | Expr1                         { $1 }

LetExpr         :: { Expr }
LetExpr         : let    VOpen Bindings VClose in Expr { Let NonRec $3 $6 }
                | letrec VOpen Bindings VClose in Expr { Let Rec $3 $6 }
                | let    '{'   Bindings '}'    in Expr { Let NonRec $3 $6 }
                | letrec '{'   Bindings '}'    in Expr { Let Rec $3 $6 }

Binders         :: { [Name] }
Binders         : Var Binders                  { $1 : $2 }
                | Var                          { [$1] }

Application     :: { Expr }
Application     : Expr1 AppArgs                 { foldl' App $1 $2 }

-- TODO: Application can probably be written as a single rule, without AppArgs
AppArgs         :: { [Expr] }
AppArgs         : Expr1 AppArgs                 { $1 : $2 }
                | Expr1                         { [$1] }

Expr1           :: { Expr }
Expr1           : litint                        { IntE $1 }
                | Id                            { Var $1 }
                | '(' Expr ')'                  { $2 }

Bindings        :: { [Binding] }
Bindings        : Binding ';' Bindings          { $1 : $3 }
                | Binding ';'                   { [$1] }
                | Binding                       { [$1] }

Binding         :: { Binding }
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
parseError :: [Located CoreToken] -> RLPC ParseError a
parseError (Located y x l _ : _) = addFatal err
    where err = SrcError
            { _errSpan       = (y,x,l)
            , _errSeverity   = Error
            , _errDiagnostic = ParErrParse
            }

parseTmp :: IO Module
parseTmp = do
    s <- readFile "/tmp/t.hs"
    case parse s of
        Left e -> error (show e)
        Right (ts,_) -> pure ts
    where
        parse = evalRLPC RLPCOptions . (lexCore >=> parseCore)

}

