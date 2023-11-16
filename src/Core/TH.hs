module Core.TH
    ( coreExpr
    , core
    )
    where
----------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
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
qCore s = case lexCore s >>= parseCore of
    Success a -> lift a
    Error e _ _ -> error e

qCoreExpr :: String -> Q Exp
qCoreExpr s = case lexCore s >>= parseCoreExpr of
    Success a -> lift a
    Error e _ _ -> error e

