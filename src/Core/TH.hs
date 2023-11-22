module Core.TH
    ( coreExpr
    , core
    )
    where
----------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax hiding (Module)
import Language.Haskell.TH.Quote
import Control.Monad                ((>=>))
import Compiler.RLPC
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
        parse = evalRLPC RLPCOptions . (lexCore >=> parseCore)

qCoreExpr :: String -> Q Exp
qCoreExpr s = case parseExpr s of
    Left e       -> error (show e)
    Right (m,ts) -> lift m
    where
        parseExpr = evalRLPC RLPCOptions . (lexCore >=> parseCoreExpr)

