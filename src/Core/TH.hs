{-|
Module      : Core.TH
Description : Core quasiquoters
-}
module Core.TH
    ( coreExpr
    , coreProg
    , core
    )
    where
----------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax   hiding (Module)
import Language.Haskell.TH.Quote
import Control.Monad                ((>=>))
import Compiler.RLPC
import Data.Default.Class           (def)
import Data.Text                    qualified as T
import Core.Parse
import Core.Lex
----------------------------------------------------------------------------------

core :: QuasiQuoter
core = QuasiQuoter
    { quoteExp = qCore
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

coreProg :: QuasiQuoter
coreProg = QuasiQuoter
    { quoteExp = qCoreProg
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

coreExpr :: QuasiQuoter
coreExpr = QuasiQuoter
    { quoteExp = qCoreExpr
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

qCore :: String -> Q Exp
qCore s = case parse (T.pack s) of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parse = evalRLPC def . (lexCore >=> parseCore)

qCoreExpr :: String -> Q Exp
qCoreExpr s = case parseExpr (T.pack s) of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parseExpr = evalRLPC def . (lexCore >=> parseCoreExpr)

qCoreProg :: String -> Q Exp
qCoreProg s = case parseProg (T.pack s) of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parseProg = evalRLPC def . (lexCore >=> parseCoreProg)

