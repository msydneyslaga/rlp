{
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Rlp.Parse
    ( parseRlpProg
    , parseRlpExpr
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
import Data.Functor.Apply
import Data.Functor.Bind
import Control.Comonad
import Data.Functor
import Data.Semigroup.Traversable
import Data.Text                    qualified as T
import Data.Void
}

%name parseRlpProg StandaloneProgram
%name parseRlpExpr StandaloneExpr

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
    '::'            { Located _ TokenHasType }
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
    let             { Located _ TokenLet }
    in              { Located _ TokenIn }

%nonassoc '='
%right '->'
%right in

%%

StandaloneProgram   :: { RlpProgram RlpcPs }
StandaloneProgram   : '{' Decls  '}'        {% mkProgram $2 }
                    | VL  DeclsV VR         {% mkProgram $2 }

StandaloneExpr      :: { RlpExpr RlpcPs }
                    : VL Expr VR            { extract $2 }

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
            | TySigDecl                 { $1 }
            | DataDecl                  { $1 }
            | InfixDecl                 { $1 }

TySigDecl   :: { Decl' RlpcPs }
            : Var '::' Type             { (\e -> TySigD [extract e]) <<~ $1 <~> $3 }

InfixDecl   :: { Decl' RlpcPs }
            : InfixWord litint InfixOp  { $1 =>> \w ->
                                          InfixD (extract $1) (extractInt $ extract $2)
                                          (extract $3) }

InfixWord   :: { Located Assoc }
            : infixl                    { $1 \$> InfixL }
            | infixr                    { $1 \$> InfixR }
            | infix                     { $1 \$> Infix  }

DataDecl    :: { Decl' RlpcPs }
            : data Con TyParams '=' DataCons    { $1 \$> DataD (extract $2) $3 $5 }

TyParams    :: { [PsName] }
            : {- epsilon -}             { [] }
            | TyParams varname          { $1 `snoc` (extractName . extract $ $2) }

DataCons    :: { [ConAlt RlpcPs] }
            : DataCons '|' DataCon      { $1 `snoc` $3 }
            | DataCon                   { [$1] }

DataCon     :: { ConAlt RlpcPs }
            : Con Type1s                { ConAlt (extract $1) $2 }

Type1s      :: { [RlpType' RlpcPs] }
            : {- epsilon -}             { [] }
            | Type1s Type1              { $1 `snoc` $2 }

Type1       :: { RlpType' RlpcPs }
            : '(' Type ')'              { $2 }
            | conname                   { fmap ConT (mkPsName $1) }
            | varname                   { fmap VarT (mkPsName $1) }

Type        :: { RlpType' RlpcPs }
            : Type '->' Type            { FunT <<~ $1 <~> $3 }
            | Type1                     { $1 }

FunDecl     :: { Decl' RlpcPs }
FunDecl     : Var Params '=' Expr       { $4 =>> \e ->
                                          FunD (extract $1) $2 e Nothing }

Params      :: { [Pat' RlpcPs] }
Params      : {- epsilon -}             { [] }
            | Params Pat1               { $1 `snoc` $2 }

Pat1        :: { Pat' RlpcPs }
            : Var                       { fmap VarP $1 }
            | Lit                       { LitP <<= $1 }

Expr        :: { RlpExpr' RlpcPs }
            : Expr1 InfixOp Expr        { $2 =>> \o ->
                                          OAppE (extract o) $1 $3 }
            | Expr1                     { $1 }
            | LetExpr                   { $1 }

LetExpr     :: { RlpExpr' RlpcPs }
            : let layout1(Binding) in Expr { $1 \$> LetE $2 $4 }

layout1(p)  : '{' layout_list1(';',p) '}'   { $2 }
            | VL  layout_list1(VS,p) VR     { $2 }

layout_list1(sep,p) : p sep                      { [$1] }
                    | p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }

Binding     :: { Binding' RlpcPs }
            : Pat1 '=' Expr              { PatB <<~ $1 <~> $3 }

Expr1       :: { RlpExpr' RlpcPs }
            : '(' Expr ')'              { $1 .> $2 <. $3 }
            | Lit                       { fmap LitE $1 }
            | Var                       { fmap VarE $1 }

InfixOp     :: { Located PsName }
            : consym                    { mkPsName $1 }
            | varsym                    { mkPsName $1 }

-- TODO: microlens-pro save me microlens-pro (rewrite this with prisms)
Lit         :: { Lit' RlpcPs }
            : litint                    { $1 <&> (IntL . (\ (TokenLitInt n) -> n)) }

Var         :: { Located PsName }
Var         : varname                   { mkPsName $1 }

Con         :: { Located PsName }
            : conname                   { mkPsName $1 }

{

mkPsName :: Located RlpToken -> Located PsName
mkPsName = fmap extractName

extractName :: RlpToken -> PsName
extractName = \case
    TokenVarName n -> n
    TokenConName n -> n
    TokenConSym  n -> n
    TokenVarSym  n -> n
    _              -> error "mkPsName: not an identifier"

extractInt :: RlpToken -> Int
extractInt (TokenLitInt n) = n
extractInt _ = error "extractInt: ugh"

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
    pure $ Located (spanFromPos pos 0) (InfixD a p n)

intOfToken :: Located RlpToken -> Int
intOfToken (Located _ (TokenLitInt n)) = n

}
