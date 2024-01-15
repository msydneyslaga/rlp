{
module Rlp.Parse
    ( parseRlpProgram
    )
    where
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
}

%name parseRlpProgram StandaloneProgram

%monad { P }
%lexer { lexerCont } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName $$) }
    litint          { Located _ (TokenLitInt $$) }
    '='             { Located _ TokenEquals }
    ';'             { Located _ TokenSemicolon }
    ';?'            { Located _ TokenSemicolonV }
    '{'             { Located _ TokenLBrace }
    '}'             { Located _ TokenRBrace }
    '{?'            { Located _ TokenLBraceV }
    '?}'            { Located _ TokenRBraceV }
    eof             { Located _ TokenEOF }

%%

StandaloneProgram   :: { [PartialDecl'] }
StandaloneProgram   : VL Decls VR eof       { $2 }

VL  :: { () }
VL  : '{?'          { () }

VR  :: { () }
VR  : '?}'          { () }
    | error         { () }

Decls               :: { [PartialDecl'] }
Decls               : Decl Semi Decls       { $1 : $3 }
                    | Decl Semi             { [$1] }
                    | Decl                  { [$1] }

Semi                :: { Located RlpToken }
Semi                : ';'                   { $1 }
                    | ';?'                  { $1 }

Decl        :: { PartialDecl' }
Decl        : FunDecl                   { undefined }

FunDecl     :: { PartialDecl' }
FunDecl     : varname '=' Expr          { undefined }

Expr        :: { RlpExpr' }
Expr        : Literal                   { LitE $1 }
            | Var                       { VarE $1 }

Literal     :: { Lit' }
Literal     : litint                    { IntL $1 }

Var         :: { VarId }
Var         : varname                   { NameVar $1 }

{

parseError :: Located RlpToken -> P a
parseError = error . show

}
