module Core.SystemF
    ( lintCoreProgR
    )
    where
--------------------------------------------------------------------------------
import Compiler.RLPC
import Core
--------------------------------------------------------------------------------

lintCoreProgR :: (Monad m) => Program Var -> RLPCT m (Program Name)
lintCoreProgR = undefined

lint :: Program Var -> Program Name
lint = undefined

