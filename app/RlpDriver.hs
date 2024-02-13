{-# LANGUAGE OverloadedStrings #-}
module RlpDriver
    ( driver
    )
    where
--------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Monad

import Rlp.Lex
import Rlp.Parse
import Rlp2Core
import GM
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = forFiles_ $ \f ->
    withSource f (parseRlpProgR >=> desugarRlpProgR >=> evalProgR)

