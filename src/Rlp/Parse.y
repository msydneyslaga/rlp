{
module Rlp.Parse
    (
    )
    where
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
}

%name rlp
%monad { P }
%lexer { lexerCont } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName $$) }
    '='             { Located _ TokenEquals }
    eof             { Located _ TokenEOF }

%%

Decl        :: { PartialDecl' }
Decl        : FunDecl                   { undefined }

FunDecl     :: { PartialDecl' }
FunDecl     : varname '=' Expr          { undefined }

Expr :: { RlpExpr' }
Expr :                  { undefined }

{

parseError :: Located RlpToken -> P a
parseError = error "aaaaah"

}
