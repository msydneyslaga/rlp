{
{-|
Module : Core.Parse
Description : Parser for the Core language
-}
{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module Core.Parse
    ( parseCore
    , parseCoreExpr
    , parseCoreProg
    , parseCoreProgR
    , module Core.Lex -- temp convenience
    , SrcError
    , Module
    )
    where

import Control.Monad        ((>=>))
import Data.Foldable        (foldl')
import Data.Functor.Identity
import Core.Syntax
import Core.Lex
import Compiler.RLPC
import Control.Monad
import Lens.Micro
import Data.Default.Class   (def)
import Data.Hashable        (Hashable)
import Data.List.Extra
import Data.Text.IO         qualified as TIO
import Data.Text            (Text)
import Data.Text            qualified as T
import Data.HashMap.Strict  qualified as H
}

%name parseCore Module
%name parseCoreExpr StandaloneExpr
%name parseCoreProg StandaloneProgram
%tokentype { Located CoreToken }
%error { parseError }
%monad { RLPC } { happyBind } { happyPure }

%token
      let             { Located _ TokenLet }
      letrec          { Located _ TokenLetrec }
      module          { Located _ TokenModule }
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

Module          :: { Module Name }
Module          : module conname where Program Eof { Module (Just ($2, [])) $4 }
                | Program Eof                      { Module Nothing $1 }

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

StandaloneProgram :: { Program Name }
StandaloneProgram : Program eof                 { $1 }

Program         :: { Program Name }
Program         : ScTypeSig ';' Program         { insTypeSig $1 $3 }
                | ScTypeSig OptSemi             { singletonTypeSig $1 }
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
ScTypeSig       : Var '::' Type                 { ($1,$3) }

ScDefs          :: { [ScDef Name] }
ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
                | ScDef ';'                     { [$1] }
                | ScDef                         { [$1] }

ScDef           :: { ScDef Name }
ScDef           : Var ParList '=' Expr          { ScDef $1 $2 $4 }
                -- hack to allow constructors to be compiled into scs
                | Con ParList '=' Expr          { ScDef $1 $2 $4 }

Type            :: { Type }
Type            : Type1                         { $1 }

Type1           :: { Type }
Type1           : '(' Type ')'                  { $2 }
                | Type1 '->' Type               { $1 :-> $3 }
                -- do we want to allow symbolic names for tyvars and tycons?
                | varname                       { TyVar $1 }
                | conname                       { TyCon $1 }

ParList         :: { [Name] }
ParList         : Var ParList                   { $1 : $2 }
                | {- epsilon -}                 { [] }

StandaloneExpr  :: { Expr Name }
StandaloneExpr  : Expr eof                      { $1 }

Expr            :: { Expr Name }
Expr            : LetExpr                       { $1 }
                | 'λ' Binders '->' Expr         { Lam $2 $4 }
                | Application                   { $1 }
                | CaseExpr                      { $1 }
                | Expr1                         { $1 }

LetExpr         :: { Expr Name }
LetExpr         : let    '{'   Bindings '}'    in Expr { Let NonRec $3 $6 }
                | letrec '{'   Bindings '}'    in Expr { Let Rec $3 $6 }

Binders         :: { [Name] }
Binders         : Var Binders                  { $1 : $2 }
                | Var                          { [$1] }

Application     :: { Expr Name }
Application     : Expr1 AppArgs                { foldl' App $1 $2 }

AppArgs         :: { [Expr Name] }
AppArgs         : Expr1 AppArgs                 { $1 : $2 }
                | Expr1                         { [$1] }

CaseExpr        :: { Expr Name }
CaseExpr        : case Expr of '{' Alters '}'   { Case $2 $5 }

Alters          :: { [Alter Name] }
Alters          : Alter ';' Alters              { $1 : $3 }
                | Alter ';'                     { [$1] }
                | Alter                         { [$1] }

Alter           :: { Alter Name }
Alter           : alttag ParList '->' Expr      { Alter (AltTag  $1) $2 $4 }
                | Con    ParList '->' Expr      { Alter (AltData $1) $2 $4 }

Expr1           :: { Expr Name }
Expr1           : litint                        { Lit $ IntL $1 }
                | Id                            { Var $1 }
                | PackCon                       { $1 }
                | '(' Expr ')'                  { $2 }

PackCon         :: { Expr Name }
PackCon         : pack '{' litint litint '}'    { Con $3 $4 }

Bindings        :: { [Binding Name] }
Bindings        : Binding ';' Bindings          { $1 : $3 }
                | Binding ';'                   { [$1] }
                | Binding                       { [$1] }

Binding         :: { Binding Name }
Binding         : Var '=' Expr                  { $1 := $3 }

Id              :: { Name }
Id              : Var                                { $1 }
                | Con                                { $1 }

Var             :: { Name }
Var             : varname                           { $1 }
                | varsym                            { $1 }

Con             :: { Name }
Con             : conname                           { $1 }
                | consym                            { $1 }

{

parseError :: [Located CoreToken] -> RLPC a
parseError (Located _ t : _) =
    error $ "<line>" <> ":" <> "<col>"
         <> ": parse error at token `" <> show t <> "'"

{-# WARNING parseError "unimpl" #-}

exprPragma :: [String] -> RLPC (Expr Name)
exprPragma ("AST" : e) = undefined
exprPragma _           = undefined

{-# WARNING exprPragma "unimpl" #-}

astPragma :: [String] -> RLPC (Expr Name)
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

parseCoreProgR :: forall m. (Monad m) => [Located CoreToken] -> RLPCT m Program'
parseCoreProgR = ddumpast <=< (hoistRlpcT generalise . parseCoreProg)
    where
        generalise :: forall a. Identity a -> m a
        generalise (Identity a) = pure a

        ddumpast :: Program' -> RLPCT m Program'
        ddumpast p = do
            addDebugMsg "dump-parsed-core" . show $ p
            pure p

happyBind :: RLPC a -> (a -> RLPC b) -> RLPC b
happyBind m k = m >>= k

happyPure :: a -> RLPC a
happyPure a = pure a

doTLPragma :: Pragma -> Program' -> RLPC Program'
-- TODO: warn unrecognised pragma
doTLPragma (Pragma []) p = pure p

doTLPragma (Pragma pr) p = case pr of
    -- TODO: warn on overwrite
    ["PackData", n, readt -> t, readt -> a] ->
        pure $ p & programDataTags . at n ?~ (t,a)

readt :: (Read a) => Text -> a
readt = read . T.unpack

}

