{
module Rlp.Parse
    ( parseRlpProgram
    , parseTest
    )
    where
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
import Data.Fix
import Data.Functor.Const
}

%name parseRlpProgram StandaloneProgram
%name parseTest VL

%monad { P }
%lexer { lexDebug } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName $$) }
    conname         { Located _ (TokenConName $$) }
    data            { Located _ TokenData }
    litint          { Located _ (TokenLitInt $$) }
    '='             { Located _ TokenEquals }
    '|'             { Located _ TokenPipe }
    ';'             { Located _ TokenSemicolon }
    vsemi           { Located _ TokenSemicolonV }
    '{'             { Located _ TokenLBrace }
    '}'             { Located _ TokenRBrace }
    vlbrace         { Located _ TokenLBraceV }
    vrbrace         { Located _ TokenRBraceV }

%%

StandaloneProgram   :: { [PartialDecl'] }
StandaloneProgram   : '{' Decls '}'         { $2 }
                    | VL  Decls VR          { $2 }

VL  :: { () }
VL  : vlbrace       { () }

VR  :: { () }
VR  : vrbrace       { () }
    | error         { () }

Decls               :: { [PartialDecl'] }
Decls               : Decl VS Decls         { $1 : $3 }
                    | Decl VS               { [$1] }
                    | Decl                  { [$1] }

Semi                :: { Located RlpToken }
Semi                : ';'                   { $1 }

VS                  :: { Located RlpToken }
VS                  : ';'                   { $1 }
                    | vsemi                 { $1 }

Decl        :: { PartialDecl' }
Decl        : FunDecl                   { $1 }

FunDecl     :: { PartialDecl' }
FunDecl     : Var '=' Expr              { FunD $1 [] (Const $3) Nothing }

Expr        :: { PartialExpr' }
Expr        : Literal                   { Fix . E $ LitEF $1 }
            | Var                       { Fix . E $ VarEF $1 }

Literal     :: { Lit' }
Literal     : litint                    { IntL $1 }

Var         :: { VarId }
Var         : varname                   { NameVar $1 }

{

parseError :: Located RlpToken -> P a
parseError = error . show

}
