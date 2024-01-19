{-|
Module      : Core.TH
Description : Core quasiquoters
-}
module Core.TH
    ( coreExpr
    , coreProg
    , coreProgT
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
import Core.Syntax                  (Expr(Var))
import Core.HindleyMilner           (checkCoreProgR)
----------------------------------------------------------------------------------

-- TODO: write in terms of a String -> QuasiQuoter

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

-- | Type-checked @coreProg@
coreProgT :: QuasiQuoter
coreProgT = QuasiQuoter
    { quoteExp = qCoreProgT
    , quotePat = error "core quasiquotes may only be used in expressions"
    , quoteType = error "core quasiquotes may only be used in expressions"
    , quoteDec = error "core quasiquotes may only be used in expressions"
    }

qCore :: String -> Q Exp
qCore s = undefined

{-# WARNING qCore "unimpl" #-}

qCoreExpr :: String -> Q Exp
qCoreExpr s = undefined

{-# WARNING qCoreExpr "unimpl" #-}

qCoreProg :: String -> Q Exp
qCoreProg s = undefined

{-# WARNING qCoreProg "unimpl" #-}

qCoreProgT :: String -> Q Exp
qCoreProgT s = undefined

