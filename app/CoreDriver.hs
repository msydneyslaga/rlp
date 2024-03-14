module CoreDriver
    ( driver
    )
    where
--------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Monad
import Data.Text                    qualified as T
import Control.Lens.Combinators

import Core.Lex
import Core.Parse
import GM
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = forFiles_ $ \f ->
    withSource f (lexCoreR >=> parseCoreProgR >=> undefined >=> evalProgR)

driverSource :: T.Text -> RLPCIO ()
driverSource = lexCoreR >=> parseCoreProgR
            >=> undefined >=> evalProgR >=> printRes
    where
        printRes = liftIO . print . view _1

