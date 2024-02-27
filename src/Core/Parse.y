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
      ':'             { Located _ TokenHasType }
      eof             { Located _ TokenEOF }

%right '->'

%%

Eof             :: { () }
Eof             : eof           { () }
                | error         { () }

StandaloneProgram :: { Program Var }
StandaloneProgram : Program eof                 { $1 }

Program         :: { Program Var }
                : TypedScDef ';' Program    { $3 & insTypeSig (fst $1)
                                                 & insScDef (snd $1) }
                | TypedScDef OptSemi        { mempty & insTypeSig (fst $1)
                                                     & insScDef (snd $1) }
                | TLPragma Program          {% doTLPragma $1 $2 }
                | TLPragma                  {% doTLPragma $1 mempty }

TLPragma        :: { Pragma }
                : '{-#' Words '#-}'             { Pragma $2 }

Words           :: { [Text] }
                : Words word                    { $1 `snoc` $2 }
                | word                          { [$1] }

OptSemi         :: { () }
OptSemi         : ';'                           { () }
                | {- epsilon -}                 { () }

ScTypeSig       :: { (Name, Type) }
ScTypeSig       : Id ':' Type                   { ($1, $3) }

TypedScDef      :: { (Var, ScDef Var) }
                : Id ':' Type ';' Id ParList '=' Expr
                            { (MkVar $1 $3, mkTypedScDef $1 $3 $5 $6 $8) }

-- ScDefs          :: { [ScDef PsName] }
-- ScDefs          : ScDef ';' ScDefs              { $1 : $3 }
--                 | ScDef ';'                     { [$1] }
--                 | ScDef                         { [$1] }
-- 
-- ScDef           :: { ScDef PsName }
-- ScDef           : Id ParList '=' Expr   { ScDef (Left $1) $2
--                                                 ($4 & binders %~ Right) }

Type            :: { Type }
                : TypeApp '->' TypeApp              { $1 :-> $3 }
                | TypeApp                         { $1 }

TypeApp         :: { Type }
                : TypeApp Type1                 { TyApp $1 $2 }
                | Type1                         { $1 }

-- do we want to allow symbolic names for tyvars and tycons?

Type1           :: { Type }
Type1           : '(' Type ')'                  { $2 }
                | varname                       { TyVar $1 }
                | conname                       { TyCon $1 }

ParList      :: { [Name] }
ParList      : varname ParList            { $1 : $2 }
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
                : '@' Type1                     { Type $2 }
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
Var             : '(' varname ':' Type ')'          { MkVar $2 $4 }

{

parseError :: [Located CoreToken] -> P a
parseError (Located _ t : _) =
    error $ "<line>" <> ":" <> "<col>"
         <> ": parse error at token `" <> show t <> "'"

exprPragma :: [String] -> RLPC (Expr Var)
exprPragma ("AST" : e) = undefined
exprPragma _           = undefined

astPragma :: [String] -> RLPC (Expr Var)
astPragma _ = undefined

-- insTypeSig :: (Hashable b) => (b, Type) -> Program b -> Program b
-- insTypeSig ts = programTypeSigs %~ uncurry H.insert ts

insTypeSig :: Var -> Program Var -> Program Var
insTypeSig w@(MkVar _ t) = programTypeSigs %~ H.insert w t

-- singletonTypeSig :: (Hashable b) => (b, Type) -> Program b
-- singletonTypeSig ts = insTypeSig ts mempty

insScDef :: (Hashable b) => ScDef b -> Program b -> Program b
insScDef sc = programScDefs %~ (sc:)

-- singletonScDef :: (Hashable b) => ScDef b -> Program b
-- singletonScDef sc = insScDef sc mempty

parseCoreExprR :: (Monad m) => [Located CoreToken] -> RLPCT m (Expr Var)
parseCoreExprR = liftMaybe . snd . flip runP def . parseCoreExpr

parseCoreProgR :: forall m. (Monad m)
               => [Located CoreToken] -> RLPCT m (Program Var)
parseCoreProgR s = do
    let p = runP (parseCoreProg s) def
    case p of
        (st, Just a) -> do
            ddumpast a
            pure a
  where
    ddumpast :: Show a => Program a -> RLPCT m (Program a)
    ddumpast p = do
        addDebugMsg "dump-parsed-core" . show $ p
        pure p

happyBind :: RLPC a -> (a -> RLPC b) -> RLPC b
happyBind m k = m >>= k

happyPure :: a -> RLPC a
happyPure a = pure a

doTLPragma :: Pragma -> Program Var -> P (Program Var)
-- TODO: warn unrecognised pragma
doTLPragma (Pragma []) p = pure p

doTLPragma (Pragma pr) p = case pr of
    -- TODO: warn on overwrite
    ["PackData", n, readt -> t, readt -> a] ->
        pure $ p & programDataTags . at n ?~ (t,a)

readt :: (Read a) => Text -> a
readt = read . T.unpack

}

