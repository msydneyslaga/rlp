module Rlp.TH
    ( rlpProg
    )
    where
--------------------------------------------------------------------------------
import Language.Haskell.TH
import Language.Haskell.TH.Syntax   hiding (Module)
import Language.Haskell.TH.Quote
import Control.Monad                ((>=>))
import Compiler.RLPC
import Data.Default.Class           (def)
import Data.Text                    qualified as T
import Rlp.Parse
--------------------------------------------------------------------------------

rlpProg :: QuasiQuoter
rlpProg = QuasiQuoter
    { quoteExp = qRlpProg
    , quotePat = error "rlp quasiquotes may only be used in expressions"
    , quoteType = error "rlp quasiquotes may only be used in expressions"
    , quoteDec = error "rlp quasiquotes may only be used in expressions"
    }

qRlpProg :: String -> Q Exp
qRlpProg s = case parse (T.pack s) of
    Nothing      -> error "error lol iddfk"
    Just a       -> lift a
    where
        parse = execP' parseRlpProg

