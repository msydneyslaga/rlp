-- TODO: resolve shift/reduce conflicts
{
module Core.Parse
    ( parseCore
    , parseCoreExpr
    , module Core.Lex -- temp convenience
    , parseTmp
    )
    where

import Data.Foldable        (foldl')
import Core.Syntax
import Core.Lex
import Compiler.RLPC
}

%name parseCore Module
%name parseCoreExpr Expr
%tokentype { CoreToken }
%error { parseError }
%monad { RLPC }

%token
      let             { TokenLet }
      letrec          { TokenLetrec }
      module          { TokenModule }
      where           { TokenWhere }
      ','             { TokenComma }
      in              { TokenIn }
      litint          { TokenLitInt $$ }
      varname         { TokenVarName $$ }
      varsym          { TokenVarSym $$ }
      conname         { TokenConName $$ }
      consym          { TokenConSym $$ }
      'λ'             { TokenLambda }
      '->'            { TokenArrow }
      '='             { TokenEquals }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }
      ';'             { TokenSemicolon }
      eof             { TokenEOF }

%%

Module          :: { Module }
Module          : module conname where Program Eof { Module (Just ($2, [])) $4 }
                | Program Eof                      { Module Nothing $1 }

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

Program         :: { Program }
Program         : '{' ScDefs Close              { Program $2 }

ScDefs          :: { [ScDef] }
ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
                | {- epsilon -}                 { [] }

ScDef           :: { ScDef }
ScDef           : Var ParList '=' Expr          { ScDef $1 $2 $4 }

ParList         :: { [Name] }
ParList         : Var ParList                   { $1 : $2 }
                | {- epsilon -}                 { [] }

Expr            :: { Expr }
Expr            : let '{' Bindings Close in Expr  { Let NonRec $3 $6 }
                | letrec '{' Bindings Close in Expr { Let Rec $3 $6 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | Expr1                         { $1 }

Close           :: { () }
Close           : '}'                           { () }
                | error                         { () }

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
parseError :: [CoreToken] -> a
parseError ts = error $ "parse error at token: " <> show (head ts)

parseTmp :: IO (Module)
parseTmp = do
    s <- readFile "/tmp/t.hs"
    case lexCore' s >>= runRLPC . parseCore of
        Left e -> error e
        Right a -> pure a
}

