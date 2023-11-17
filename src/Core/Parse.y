{
module Core.Parse
    ( parseCore
    -- , parseCoreExpr
    , module Core.Lex -- temp convenience
    )
    where

import Data.Foldable        (foldl')
import Core.Syntax
import Core.Lex
}

%name parseCore
%name parseCoreExpr Expr
%tokentype { CoreToken }
%error { parseError }

%token
      let             { TokenLet }
      letrec          { TokenLetrec }
      module          { TokenModule }
      where           { TokenWhere }
      ','             { TokenComma }
      in              { TokenIn }
      litint          { TokenLitInt $$ }
      name            { TokenName $$ }
      sym             { TokenSym $$ }
      'λ'             { TokenLambda }
      '->'            { TokenArrow }
      '='             { TokenEquals }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }
      ';'             { TokenSemicolon }

%%

ExportList      :: { [Name] }
ExportList      : '(' Exports ')'               { $2 }

Exports         :: { [Name] }
Exports         : Var ',' Exports               { $1 : $3 }
                | Var                           { [$1] }

Expr            :: { Expr }
Expr            : let Bindings in Expr          { Let NonRec $2 $4 }
                | letrec Bindings in Expr       { Let Rec $2 $4 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | Expr1                         { $1 }

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
                | Var                           { Var $1 }
                | '(' Expr ')'                  { $2 }

Var            :: { Name }
Var            : '(' sym ')'                    { $2 }
               | name                           { $1 }

Bindings        :: { [Binding] }
Bindings        : Var '=' Expr                  { [$1 := $3] }

{
parseError :: [CoreToken] -> a
parseError _ = error "fuuckk!"
}

