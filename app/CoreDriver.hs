module CoreDriver
    ( driver
    )
    where
--------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Monad
import Data.Text                    qualified as T
import Lens.Micro.Platform

import Core.Lex
import Core.Parse
import GM
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = forFiles_ $ \f ->
    withSource f (lexCoreR >=> parseCoreProgR >=> evalProgR)

driverSource :: T.Text -> RLPCIO ()
driverSource = lexCoreR >=> parseCoreProgR >=> evalProgR >=> printRes
    where
        printRes = liftIO . print . view _1

