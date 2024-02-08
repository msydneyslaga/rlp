{
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Rlp.Parse
    ( parseRlpProg
    , parseRlpProgR
    , parseRlpExpr
    , parseRlpExprR
    )
    where
import Compiler.RlpcError
import Compiler.RLPC
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
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.Void
import Compiler.Types
}

%name parseRlpProg StandaloneProgram
%name parseRlpExpr StandaloneExpr

%monad { P }
%lexer { lexCont } { Located _ TokenEOF }
%error { parseError }
%errorhandlertype explist
%tokentype { Located RlpToken }

%token
    varname         { Located _ (TokenVarName _) }
    conname         { Located _ (TokenConName _) }
    consym          { Located _ (TokenConSym _) }
    varsym          { Located _ (TokenVarSym _) }
    data            { Located _ TokenData }
    case            { Located _ TokenCase }
    of              { Located _ TokenOf }
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
                    | {- epsilon -}         { [] }

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
            | TypeApp                   { $1 }

TypeApp     :: { RlpType' RlpcPs }
            : Type1                     { $1 }
            | TypeApp Type1             { AppT <<~ $1 <~> $2 }

FunDecl     :: { Decl' RlpcPs }
FunDecl     : Var Params '=' Expr       { $4 =>> \e ->
                                          FunD (extract $1) $2 e Nothing }

Params      :: { [Pat' RlpcPs] }
Params      : {- epsilon -}             { [] }
            | Params Pat1               { $1 `snoc` $2 }

Pat         :: { Pat' RlpcPs }
            : Con Pat1s                 { $1 =>> \cn ->
                                          ConP (extract $1) $2 }
            | Pat1                      { $1 }

Pat1s       :: { [Pat' RlpcPs] }
            : Pat1s Pat1                { $1 `snoc` $2 }
            | Pat1                      { [$1] }

Pat1        :: { Pat' RlpcPs }
            : Con                       { fmap (`ConP` []) $1 }
            | Var                       { fmap VarP $1 }
            | Lit                       { LitP <<= $1 }
            | '(' Pat ')'               { $1 .> $2 <. $3 }

Expr        :: { RlpExpr' RlpcPs }
            -- infixities delayed till next release :(
            -- : Expr1 InfixOp Expr        { $2 =>> \o ->
            --                              OAppE (extract o) $1 $3 }
            : TempInfixExpr             { $1 }
            | LetExpr                   { $1 }
            | CaseExpr                  { $1 }
            | AppExpr                   { $1 }

TempInfixExpr :: { RlpExpr' RlpcPs }
TempInfixExpr : Expr1 InfixOp TempInfixExpr {% tempInfixExprErr $1 $3 }
              | Expr1 InfixOp Expr1 { $2 =>> \o ->
                                      OAppE (extract o) $1 $3 }

AppExpr     :: { RlpExpr' RlpcPs }
            : Expr1                     { $1 }
            | AppExpr Expr1             { AppE <<~ $1 <~> $2 }

LetExpr     :: { RlpExpr' RlpcPs }
            : let layout1(Binding) in Expr { $1 \$> LetE $2 $4 }

CaseExpr    :: { RlpExpr' RlpcPs }
            : case Expr of layout0(CaseAlt)
                { CaseE <<~ $2 <#> $4 }

-- TODO: where-binds
CaseAlt     :: { (Alt RlpcPs, Where RlpcPs) }
            : Alt                           { ($1, []) }

Alt         :: { Alt RlpcPs }
            : Pat '->' Expr                 { AltA $1 $3 }

-- layout0(p : β) :: [β]
layout0(p)  : '{' layout_list0(';',p) '}'   { $2 }
            | VL  layout_list0(VS,p)  VR    { $2 }

-- layout_list0(sep : α, p : β) :: [β]
layout_list0(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }
                    | {- epsilon -}              { [] }

-- layout1(p : β) :: [β]
layout1(p)  : '{' layout_list1(';',p) '}'   { $2 }
            | VL  layout_list1(VS,p) VR     { $2 }

-- layout_list1(sep : α, p : β) :: [β]
layout_list1(sep,p) : p                          { [$1] }
                    | layout_list1(sep,p) sep p  { $1 `snoc` $3 }

Binding     :: { Binding' RlpcPs }
            : Pat '=' Expr              { PatB <<~ $1 <~> $3 }

Expr1       :: { RlpExpr' RlpcPs }
            : '(' Expr ')'              { $1 .> $2 <. $3 }
            | Lit                       { fmap LitE $1 }
            | Var                       { fmap VarE $1 }
            | Con                       { fmap VarE $1 }

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

parseRlpExprR :: (Monad m) => Text -> RLPCT m (RlpExpr RlpcPs)
parseRlpExprR s = liftErrorful $ pToErrorful parseRlpExpr st
    where
        st = programInitState s

parseRlpProgR :: (Monad m) => Text -> RLPCT m (RlpProgram RlpcPs)
parseRlpProgR s = do
    a <- liftErrorful $ pToErrorful parseRlpProg st
    addDebugMsg @_ @String "dump-parsed" $ show a
    pure a
  where
        st = programInitState s

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

parseError :: (Located RlpToken, [String]) -> P a
parseError ((Located ss t), exp) = addFatal $
    errorMsg ss (RlpParErrUnexpectedToken t exp)

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

tempInfixExprErr :: RlpExpr' RlpcPs -> RlpExpr' RlpcPs -> P a
tempInfixExprErr (Located a _) (Located b _) =
    addFatal $ errorMsg (a <> b) $ RlpParErrOther
        [ "The rl' frontend is currently in beta. Support for infix expressions is minimal, sorry! :("
        , "In the mean time, don't mix any infix operators."
        ]

}

