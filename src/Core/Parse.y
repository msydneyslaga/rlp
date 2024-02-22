{
{-|
Module : Core.Parse
Description : Parser for the Core language
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Core.Parse
    ( parseCoreExpr
    , parseCoreExprR
    , parseCoreProg
    , parseCoreProgR
    , module Core.Lex -- temp convenience
    , SrcError
    )
    where

import Control.Monad        ((>=>))
import Control.Monad.Utils  (generalise)
import Data.Foldable        (foldl')
import Data.Functor.Identity
import Core.Syntax
import Core.Lex
import Compiler.RLPC
import Control.Monad
import Control.Lens         hiding (snoc)
import Data.Default.Class   (def)
import Data.Hashable        (Hashable)
import Data.List.Extra
import Data.Text.IO         qualified as TIO
import Data.Text            (Text)
import Data.Text            qualified as T
import Data.HashMap.Strict  qualified as H

import Core.Parse.Types
}

%name parseCoreExpr StandaloneExpr
%name parseCoreProg StandaloneProgram
%tokentype { Located CoreToken }
%error { parseError }
%monad { P }

%token
      let             { Located _ TokenLet }
      letrec          { Located _ TokenLetrec }
      where           { Located _ TokenWhere }
      case            { Located _ TokenCase }
      of              { Located _ TokenOf }
      pack            { Located _ TokenPack } -- temp
      in              { Located _ TokenIn }
      litint          { Located _ (TokenLitInt $$) }
      varname         { Located _ (TokenVarName $$) }
      varsym          { Located _ (TokenVarSym $$) }
      conname         { Located _ (TokenConName $$) }
      consym          { Located _ (TokenConSym $$) }
      alttag          { Located _ (TokenAltTag $$) }
      word            { Located _ (TokenWord $$) }
      'λ'             { Located _ TokenLambda }
      '->'            { Located _ TokenArrow }
      '='             { Located _ TokenEquals }
      '@'             { Located _ TokenTypeApp }
      '('             { Located _ TokenLParen }
      ')'             { Located _ TokenRParen }
      '{'             { Located _ TokenLBrace }
      '}'             { Located _ TokenRBrace }
      '{-#'           { Located _ TokenLPragma }
      '#-}'           { Located _ TokenRPragma }
      ';'             { Located _ TokenSemicolon }
      '::'            { Located _ TokenHasType }
      eof             { Located _ TokenEOF }

%%

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

StandaloneProgram :: { Program PsName }
StandaloneProgram : Program eof                 { $1 }

Program         :: { Program PsName }
Program         : ScTypeSig ';' Program         { insTypeSig ($1 & _1 %~ Left) $3 }
                | ScTypeSig OptSemi             { singletonTypeSig ($1 & _1 %~ Left) }
                | ScDef     ';' Program         { insScDef $1 $3 }
                | ScDef     OptSemi             { singletonScDef $1 }
                | TLPragma  Program             {% doTLPragma $1 $2 }
                | TLPragma                      {% doTLPragma $1 mempty }

TLPragma        :: { Pragma }
                : '{-#' Words '#-}'             { Pragma $2 }

Words           :: { [Text] }
                : Words word                    { $1 `snoc` $2 }
                | word                          { [$1] }

OptSemi         :: { () }
OptSemi         : ';'                           { () }
                | {- epsilon -}                 { () }

ScTypeSig       :: { (Name, Type) }
ScTypeSig       : Id '::' Type                  { ($1, $3 TyKindType) }

ScDefs          :: { [ScDef PsName] }
ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
                | ScDef ';'                     { [$1] }
                | ScDef                         { [$1] }

ScDef           :: { ScDef PsName }
ScDef           : Id ParList '=' Expr          { ScDef (Left $1) $2
                                                  ($4 & _binders %~ Right) }

Type            :: { Kind -> Type }
                : Type1 '->' Type   { \case
                                        TyKindType ->
                                          $1 TyKindType :-> $3 TyKindType
                                        _ -> error "kind mismatch" }
                | Type1                         { $1 }

-- do we want to allow symbolic names for tyvars and tycons?

Type1           :: { Kind -> Type }
Type1           : '(' Type ')'                  { $2 }
                | varname                       { \k -> TyVar $ MkVar $1 k }
                | conname                       { \k -> TyCon $ MkTyCon $1 k }

ParList         :: { [PsName] }
ParList         : varname ParList               { Left $1 : $2 }
                | {- epsilon -}                 { [] }

StandaloneExpr  :: { Expr Var }
StandaloneExpr  : Expr eof                      { $1 }

Expr            :: { Expr Var }
Expr            : LetExpr                       { $1 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | CaseExpr                      { $1 }
                | Expr1                         { $1 }

LetExpr         :: { Expr Var }
LetExpr         : let    '{'   Bindings '}'    in Expr { Let NonRec $3 $6 }
                | letrec '{'   Bindings '}'    in Expr { Let Rec $3 $6 }

Binders         :: { [Var] }
Binders         : Var Binders                  { $1 : $2 }
                | Var                          { [$1] }

Application     :: { Expr Var }
Application     : Application AppArg           { App $1 $2 }
                | Expr1       AppArg           { App $1 $2 }

AppArg          :: { Expr Var }
                : '@' Type1                     { Type ($2 TyKindInferred) }
                | Expr1                         { $1 }

CaseExpr        :: { Expr Var }
CaseExpr        : case Expr of '{' Alters '}'   { Case $2 $5 }

Alters          :: { [Alter Var] }
Alters          : Alter ';' Alters              { $1 : $3 }
                | Alter ';'                     { [$1] }
                | Alter                         { [$1] }

Alter           :: { Alter Var }
Alter           : alttag  AltParList '->' Expr     { Alter (AltTag  $1) $2 $4 }
                | conname AltParList '->' Expr     { Alter (AltData $1) $2 $4 }

AltParList      :: { [Var] }
                : Var AltParList                { $1 : $2 }
                | {- epsilon -}                 { [] }

Expr1           :: { Expr Var }
Expr1           : litint                        { Lit $ IntL $1 }
                | Id                            { Var $1 }
                | PackCon                       { $1 }
                | '(' Expr ')'                  { $2 }

PackCon         :: { Expr Var }
PackCon         : pack '{' litint litint '}'    { Con $3 $4 }

Bindings        :: { [Binding Var] }
Bindings        : Binding ';' Bindings          { $1 : $3 }
                | Binding ';'                   { [$1] }
                | Binding                       { [$1] }

Binding         :: { Binding Var }
Binding         : Var '=' Expr                  { $1 := $3 }

Id              :: { Name }
                : varname                           { $1 }
                | conname                           { $1 }

Var             :: { Var }
Var             : '(' varname '::' Type ')'         { MkVar $2 ($4 TyKindType) }

{

parseError :: [Located CoreToken] -> P a
parseError (Located _ t : _) =
    error $ "<line>" <> ":" <> "<col>"
         <> ": parse error at token `" <> show t <> "'"

{-# WARNING parseError "unimpl" #-}

exprPragma :: [String] -> RLPC (Expr Var)
exprPragma ("AST" : e) = undefined
exprPragma _           = undefined

{-# WARNING exprPragma "unimpl" #-}

astPragma :: [String] -> RLPC (Expr Var)
astPragma _ = undefined

{-# WARNING astPragma "unimpl" #-}

insTypeSig :: (Hashable b) => (b, Type) -> Program b -> Program b
insTypeSig ts = programTypeSigs %~ uncurry H.insert ts

singletonTypeSig :: (Hashable b) => (b, Type) -> Program b
singletonTypeSig ts = insTypeSig ts mempty

insScDef :: (Hashable b) => ScDef b -> Program b -> Program b
insScDef sc = programScDefs %~ (sc:)

singletonScDef :: (Hashable b) => ScDef b -> Program b
singletonScDef sc = insScDef sc mempty

parseCoreExprR :: (Monad m) => [Located CoreToken] -> RLPCT m (Expr Var)
parseCoreExprR = liftMaybe . snd . flip runP def . parseCoreExpr

parseCoreProgR :: forall m. (Monad m)
               => [Located CoreToken] -> RLPCT m (Program PsName)
parseCoreProgR s = ddumpast =<< (liftMaybe . snd $ runP (parseCoreProg s) def)
    where
        ddumpast :: (Program PsName) -> RLPCT m (Program PsName)
        ddumpast p = do
            addDebugMsg "dump-parsed-core" . show $ p
            pure p

happyBind :: RLPC a -> (a -> RLPC b) -> RLPC b
happyBind m k = m >>= k

happyPure :: a -> RLPC a
happyPure a = pure a

doTLPragma :: Pragma -> Program PsName -> P (Program PsName)
-- TODO: warn unrecognised pragma
doTLPragma (Pragma []) p = pure p

doTLPragma (Pragma pr) p = case pr of
    -- TODO: warn on overwrite
    ["PackData", n, readt -> t, readt -> a] ->
        pure $ p & programDataTags . at n ?~ (t,a)

readt :: (Read a) => Text -> a
readt = read . T.unpack

type PsName = Either Name Var

}

