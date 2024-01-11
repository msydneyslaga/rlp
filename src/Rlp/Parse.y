{
module Rlp.Parse
    (
    )
    where
import Rlp.Lex
}

%name rlp
%monad { P }
%lexer { lexer } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    t { Located _ _ }

%%

P :: { () }
P : { error "aa" }

{

parseError :: Located RlpToken -> P a
parseError = error "aaaaah"

}
