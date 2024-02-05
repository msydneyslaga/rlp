module Rlp.TH
    ( rlpProg
    , rlpExpr
    )
    where
--------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote
import Data.Text                        (Text)
import Data.Text                        qualified as T
import Control.Monad.IO.Class
import Control.Monad

import Compiler.RLPC
import Rlp.Parse
--------------------------------------------------------------------------------

rlpProg :: QuasiQuoter
rlpProg = mkqq parseRlpProgR

rlpExpr :: QuasiQuoter
rlpExpr = mkqq parseRlpExprR

mkq :: (Lift a) => (Text -> RLPCIO a) -> String -> Q Exp
mkq parse = evalAndParse >=> lift where
    evalAndParse = liftIO . evalRLPCIO def . parse . T.pack

mkqq :: (Lift a) => (Text -> RLPCIO a) -> QuasiQuoter
mkqq p = QuasiQuoter
    { quoteExp = mkq p
    , quotePat = error "rlp quasiquotes may only be used in expressions"
    , quoteType = error "rlp quasiquotes may only be used in expressions"
    , quoteDec = error "rlp quasiquotes may only be used in expressions"
    }

