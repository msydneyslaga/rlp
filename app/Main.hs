{-# LANGUAGE BlockArguments #-}
module Main where
----------------------------------------------------------------------------------
import Compiler.RLPC
import Options.Applicative      hiding (ParseError)
import Control.Monad
import Control.Monad.Reader
import System.IO
import Core
import TIM
import Lens.Micro
import Lens.Micro.Mtl
----------------------------------------------------------------------------------

optParser :: ParserInfo RLPCOptions
optParser = info (helper <*> options)
    (  fullDesc
    <> progDesc "Compile rl' programs"
    <> header "rlpc - The Inglorious rl' Compiler"
    )

options :: Parser RLPCOptions
options = RLPCOptions
    <$> optional # strOption
        (  long "log"
        <> short 'l'
        <> metavar "FILE"
        <> help "output dumps to FILE. stderr is used by default"
        )
    -- temp. i want gcc/ghc style options
    <*> switch 
        (  long "dump-evals"
        <> short 'd'
        <> help "dump evaluation logs"
        )
    <*> some (argument str (metavar "FILES..."))
    where
        infixr 9 #
        f # x = f x

main :: IO ()
main = do
    opts <- execParser optParser
    evalRLPCIO opts driver
    pure ()

driver :: RLPCIO () ()
driver = sequence_
    [ dumpEval
    ]

whenView :: (MonadReader s m) => Getting Bool s Bool -> m () -> m ()
whenView l m = view l >>= \a -> when a m

dumpEval :: RLPCIO () ()
dumpEval = whenView rlpcDumpEval do
    fs <- view rlpcInputFiles
    forM_ fs $ \f -> liftIO (readFile f) >>= doProg 

    where
        doProg :: String -> RLPCIO () ()
        doProg s = ask >>= \o -> case parseProg o s of
            -- TODO: error handling
            Left e -> error $ show e
            Right (a,_) -> do
                log <- view rlpcLogFile
                case log of
                    Just f  -> void . liftIO $ withFile f WriteMode $ hdbgProg a
                    Nothing -> void . liftIO $ hdbgProg a stderr

        parseProg :: RLPCOptions
                  -> String
                  -> Either (SrcError ParseError) (Program, [SrcError ParseError])
        parseProg o = evalRLPC o . (lexCore >=> parseCoreProg)

