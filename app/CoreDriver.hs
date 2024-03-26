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
import Core.SystemF
import GM
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = forFiles_ $ \f ->
    withSource f (lexCoreR >=> parseCoreProgR >=> lintCoreProgR >=> evalProgR)

driverSource :: T.Text -> RLPCIO ()
driverSource = lexCoreR >=> parseCoreProgR
           >=> lintCoreProgR >=> evalProgR >=> printRes
    where
        printRes = liftIO . print . view _1

