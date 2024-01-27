{
{-# LANGUAGE LambdaCase #-}
module Rlp.Parse
    ( parseRlpProg
    )
    where
import Compiler.RlpcError
import Rlp.Lex
import Rlp.Syntax
import Rlp.Parse.Types
import Rlp.Parse.Associate
import Lens.Micro.Platform
import Data.List.Extra
import Data.Fix
import Data.Functor.Const
import Data.Functor
import Data.Text                    qualified as T
import Data.Void
}

%name parseRlpProg StandaloneProgram

%monad { P }
%lexer { lexCont } { Located _ TokenEOF }
%error { parseError }
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName _) }
    conname         { Located _ (TokenConName _) }
    consym          { Located _ (TokenConSym _) }
    varsym          { Located _ (TokenVarSym _) }
    data            { Located _ TokenData }
    litint          { Located _ (TokenLitInt _) }
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

StandaloneProgram   :: { RlpProgram RlpcPs }
StandaloneProgram   : '{' Decls  '}'        {% mkProgram $2 }
                    | VL  DeclsV VR         {% mkProgram $2 }

VL  :: { () }
VL  : vlbrace       { () }

VR  :: { () }
VR  : vrbrace       { () }
    | error         { () }

Decls               :: { [Decl' RlpcPs] }
Decls               : Decl ';' Decls        { $1 : $3 }
                    | Decl ';'              { [$1] }
                    | Decl                  { [$1] }

DeclsV              :: { [Decl' RlpcPs] }
DeclsV              : Decl VS Decls         { $1 : $3 }
                    | Decl VS               { [$1] }
                    | Decl                  { [$1] }

VS                  :: { Located RlpToken }
VS                  : ';'                   { $1 }
                    | vsemi                 { $1 }

Decl        :: { Decl' RlpcPs }
            : FunDecl                   { $1 }
            | DataDecl                  { $1 }
            | InfixDecl                 { $1 }

InfixDecl   :: { Decl' RlpcPs }
            : InfixWord litint InfixOp  {% mkInfixD $1 $2 $3 }

InfixWord   :: { Assoc }
            : infixl                    { InfixL }
            | infixr                    { InfixR }
            | infix                     { Infix  }

DataDecl    :: { Decl' RlpcPs }
            : data Con TyParams '=' DataCons    { DataD $2 $3 $5 }

TyParams    :: { [PsName] }
            : {- epsilon -}             { [] }
            | TyParams varname          { $1 `snoc` $2 }

DataCons    :: { [ConAlt RlpcPs] }
            : DataCons '|' DataCon      { $1 `snoc` $3 }
            | DataCon                   { [$1] }

DataCon     :: { ConAlt RlpcPs }
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

FunDecl     :: { Decl' RlpcPs }
FunDecl     : Var Params '=' Expr       { FunD undefined $2 $4 Nothing }

Params      :: { [Pat' RlpcPs] }
Params      : {- epsilon -}             { [] }
            | Params Pat1               { $1 `snoc` $2 }

Pat1        :: { Pat' RlpcPs }
            : Var                       { undefined }
            | Lit                       { LitP <$> $1 }

Expr        :: { RlpExpr' RlpcPs }
            : Expr1 varsym Expr         { undefined }
            | Expr1                     { $1 }

Expr1       :: { RlpExpr' RlpcPs }
            : '(' Expr ')'              { fmap ParE' $2 }
            | Lit                       { fmap LitE' $1 }
            | Var                       { fmap VarE' $1 }

-- TODO: happy prefers left-associativity. doing such would require adjusting
-- the code in Rlp.Parse.Associate to expect left-associative input rather than
-- right.
InfixExpr   :: { RlpExpr' RlpcPs }
            : Expr1 varsym Expr         { undefined }

InfixOp     :: { PsName }
            : consym                    { undefined }
            | varsym                    { undefined }

-- TODO: microlens-pro save me microlens-pro (rewrite this with prisms)
Lit         :: { Lit' RlpcPs }
            : litint                    { $1 <&> (IntL . (\ (TokenLitInt n) -> n)) }

Var         :: { Located PsName }
Var         : varname                   { mkPsName $1 }

Con         :: { Located PsName }
            : conname                   { mkPsName $1 }

{

mkPsName :: Located RlpToken -> Located PsName
mkPsName = fmap $ \case
    TokenVarName n -> n
    TokenConName n -> n
    TokenConSym  n -> n
    TokenVarSym  n -> n
    _              -> error "mkPsName: not an identifier"

mkProgram :: [Decl' RlpcPs] -> P (RlpProgram RlpcPs)
mkProgram ds = do
    pt <- use psOpTable
    pure $ RlpProgram (associate pt <$> ds)

parseError :: Located RlpToken -> P a
parseError (Located (l,c,a,s) t) = addFatal $
    errorMsg (SrcSpan l c a s) RlpParErrUnexpectedToken

mkInfixD :: Assoc -> Int -> PsName -> P (Decl' RlpcPs)
mkInfixD a p n = do
    let opl :: Lens' ParseState (Maybe OpInfo)
        opl = psOpTable . at n
    opl <~ (use opl >>= \case
        Just o  -> addWoundHere l e >> pure (Just o) where
            e = RlpParErrDuplicateInfixD n
            l = T.length n
        Nothing -> pure (Just (a,p))
        )
    pos <- use (psInput . aiPos)
    pure $ Located (spanFromPos pos 0) (InfixD' a p n)

}
