module CoreDriver
    ( driver
    )
    where
--------------------------------------------------------------------------------

driver :: RLPCIO ()
driver = undefined

parseProg :: RLPCOptions
          -> Text
          -> (Maybe Program', [MsgEnvelope RlpcError])
parseProg o = lexCoreR >=> parseCoreProgR

