{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
----------------------------------------------------------------------------------
import Compiler.RLPC
import Options.Applicative      hiding (ParseError)
import Control.Monad
import Control.Monad.Reader
import Data.HashSet             qualified as S
import System.IO
import System.Exit              (exitSuccess)
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
    <*> fmap S.fromList # many # option debugFlagReader
        (  short 'd'
        <> help "dump evaluation logs"
        <> metavar "DEBUG FLAG"
        )
    <*> some (argument str (metavar "FILES..."))
    where
        infixr 9 #
        f # x = f x

mmany :: (Alternative f, Monoid m) => f m -> f m
mmany v = liftA2 (<>) v (mmany v)

debugFlagReader :: ReadM DebugFlag
debugFlagReader = maybeReader $ Just . \case
    "dump-eval"  -> DDumpEval
    "dump-opts"  -> DDumpOpts

----------------------------------------------------------------------------------

-- temp
data CompilerError = CompilerError String

main :: IO ()
main = do
    opts <- execParser optParser
    (_, es) <- evalRLPCIO opts driver
    forM_ es $ \ (CompilerError e) -> print $ "warning: " <> e
    pure ()

driver :: RLPCIO CompilerError ()
driver = sequence_
    [ dshowFlags
    , ddumpEval
    ]

dshowFlags :: RLPCIO CompilerError ()
dshowFlags = whenFlag flagDDumpOpts do
    ask >>= liftIO . print
    liftIO $ exitSuccess

ddumpEval :: RLPCIO CompilerError ()
ddumpEval = whenFlag flagDDumpEval do
    fs <- view rlpcInputFiles
    forM_ fs $ \f -> liftIO (readFile f) >>= doProg

    where
        doProg :: String -> RLPCIO CompilerError ()
        doProg s = ask >>= \o -> case parseProg o s of
            -- TODO: error handling
            Left e -> addFatal . CompilerError $ show e
            Right (a,_) -> do
                log <- view rlpcLogFile
                case log of
                    Just f  -> void . liftIO $ withFile f WriteMode $ hdbgProg a
                    Nothing -> void . liftIO $ hdbgProg a stderr

        parseProg :: RLPCOptions
                  -> String
                  -> Either SrcError (Program, [SrcError])
        parseProg o = evalRLPC o . (lexCore >=> parseCoreProg)

