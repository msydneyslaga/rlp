module Core.TH
    ( coreExpr
    , coreProg
    , core
    )
    where
----------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (Module)
import Language.Haskell.TH.Quote
import Control.Monad                ((>=>))
import Compiler.RLPC
import Data.Default.Class           (def)
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
qCore s = case parse s of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parse = evalRLPC def . (lexCore >=> parseCore)

qCoreExpr :: String -> Q Exp
qCoreExpr s = case parseExpr s of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parseExpr = evalRLPC def . (lexCore >=> parseCoreExpr)

qCoreProg :: String -> Q Exp
qCoreProg s = case parseProg s of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parseProg = evalRLPC def . (lexCore >=> parseCoreProg)

