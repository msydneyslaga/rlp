module CoreDriver
    ( driver
    )
    where
--------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Monad

import Core.Lex
import Core.Parse
import GM
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = forFiles_ $ \f ->
    withSource f (lexCoreR >=> parseCoreProgR >=> evalProgR)

