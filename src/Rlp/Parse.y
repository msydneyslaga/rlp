{
{-# LANGUAGE LambdaCase #-}
module Rlp.Parse
    ( parseRlpProg
    )
    where
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
import Rlp.Parse.Associate
import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.Platform          ()
import Data.List.Extra
import Data.Fix
import Data.Functor.Const
}

%name parseRlpProg StandaloneProgram

%monad { P }
%lexer { lexCont } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName $$) }
    conname         { Located _ (TokenConName $$) }
    consym          { Located _ (TokenConSym $$) }
    varsym          { Located _ (TokenVarSym $$) }
    data            { Located _ TokenData }
    litint          { Located _ (TokenLitInt $$) }
    '='             { Located _ TokenEquals }
    '|'             { Located _ TokenPipe }
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

%right '->'

%%

StandaloneProgram   :: { RlpProgram' }
StandaloneProgram   : '{' Decls  '}'        {% mkProgram $2 }
                    | VL  DeclsV VR         {% mkProgram $2 }

VL  :: { () }
VL  : vlbrace       { () }

VR  :: { () }
VR  : vrbrace       { () }
    | error         { () }

Decls               :: { [PartialDecl'] }
Decls               : Decl ';' Decls        { $1 : $3 }
                    | Decl ';'              { [$1] }
                    | Decl                  { [$1] }

DeclsV              :: { [PartialDecl'] }
DeclsV              : Decl VS Decls         { $1 : $3 }
                    | Decl VS               { [$1] }
                    | Decl                  { [$1] }

VS                  :: { Located RlpToken }
VS                  : ';'                   { $1 }
                    | vsemi                 { $1 }

Decl        :: { PartialDecl' }
            : FunDecl                   { $1 }
            | DataDecl                  { $1 }
            | InfixDecl                 { $1 }

InfixDecl   :: { PartialDecl' }
            : InfixWord litint InfixOp  {% mkInfixD $1 $2 $3 }

InfixWord   :: { Assoc }
            : infixl                    { InfixL }
            | infixr                    { InfixR }
            | infix                     { Infix  }

DataDecl    :: { PartialDecl' }
            : data Con TyParams '=' DataCons    { DataD $2 $3 $5 }

TyParams    :: { [Name] }
            : {- epsilon -}             { [] }
            | TyParams varname          { $1 `snoc` $2 }

DataCons    :: { [ConAlt] }
            : DataCons '|' DataCon      { $1 `snoc` $3 }
            | DataCon                   { [$1] }

DataCon     :: { ConAlt }
            : Con Type1s                { ConAlt $1 $2 }

Type1s      :: { [Type] }
            : {- epsilon -}             { [] }
            | Type1s Type1              { $1 `snoc` $2 }

Type1       :: { Type }
            : '(' Type ')'              { $2 }
            | conname                   { TyCon $1 }
            | varname                   { TyVar $1 }

Type        :: { Type }
            : Type '->' Type            { $1 :-> $3 }
            | Type1                     { $1 }

FunDecl     :: { PartialDecl' }
FunDecl     : Var Params '=' Expr       { FunD $1 $2 (Const $4) Nothing }

Params      :: { [Pat'] }
Params      : {- epsilon -}             { [] }
            | Params Pat1               { $1 `snoc` $2 }

Pat1        :: { Pat' }
            : Var                       { VarP $1 }
            | Lit                       { LitP $1 }

Expr        :: { PartialExpr' }
            : Expr1 varsym Expr         { Fix $ B $2 (unFix $1) (unFix $3) }
            | Expr1                     { $1 }

Expr1       :: { PartialExpr' }
            : '(' Expr ')'              { wrapFix . Par . unwrapFix $ $2 }
            | Lit                       { Fix . E $ LitEF $1 }
            | Var                       { Fix . E $ VarEF $1 }

-- TODO: happy prefers left-associativity. doing such would require adjusting
-- the code in Rlp.Parse.Associate to expect left-associative input rather than
-- right.
InfixExpr   :: { PartialExpr' }
            : Expr1 varsym Expr         { Fix $ B $2 (unFix $1) (unFix $3) }

InfixOp     :: { Name }
            : consym                    { $1 }
            | varsym                    { $1 }

Lit         :: { Lit' }
Lit         : litint                    { IntL $1 }

Var         :: { VarId }
Var         : varname                   { NameVar $1 }

Con         :: { ConId }
            : conname                   { NameCon $1 }

{

mkProgram :: [PartialDecl'] -> P RlpProgram'
mkProgram ds = do
    pt <- use psOpTable
    pure $ RlpProgram (associate pt <$> ds)

parseError :: Located RlpToken -> P a
parseError (Located ((l,c),s) t) = addFatal RlpParErrUnknown

mkInfixD :: Assoc -> Int -> Name -> P PartialDecl'
mkInfixD a p n = do
    let opl :: Lens' ParseState (Maybe OpInfo)
        opl = psOpTable . at n
    opl <~ (use opl >>= \case
        -- TODO: non-fatal error
        Just o  -> pure (Just o)
        Nothing -> pure (Just (a,p))
        )
    pure $ InfixD a p n
}
