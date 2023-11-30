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
import GM
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
    {- --log, -l -}
    <$> optional # strOption
        (  long "log"
        <> short 'l'
        <> metavar "FILE"
        <> help "output dumps to FILE. stderr is used by default"
        )
    {- -d -}
    <*> fmap S.fromList # many # option debugFlagReader
        (  short 'd'
        <> help "dump evaluation logs"
        <> metavar "DEBUG FLAG"
        )
    {- --evaluator, -e -}
    <*> option evaluatorReader
        (  long "evaluator"
        <> short 'e'
        <> metavar "gm|tim"
        <> value EvaluatorGM
        <> help "the intermediate layer used to model evaluation"
        )
    <*> some (argument str (metavar "FILES..."))
    where
        infixr 9 #
        f # x = f x

evaluatorReader :: ReadM Evaluator
evaluatorReader = maybeReader $ \case
    "gm"  -> Just EvaluatorGM
    "tim" -> Just EvaluatorTIM
    _     -> Nothing

mmany :: (Alternative f, Monoid m) => f m -> f m
mmany v = liftA2 (<>) v (mmany v)

debugFlagReader :: ReadM DebugFlag
debugFlagReader = maybeReader $ \case
    "dump-eval"  -> Just DDumpEval
    "dump-opts"  -> Just DDumpOpts
    _            -> Nothing

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
                dumpEval <- chooseEval
                case log of
                    Just f  -> liftIO $ withFile f WriteMode $ dumpEval a
                    Nothing -> liftIO $ dumpEval a stderr

        parseProg :: RLPCOptions
                  -> String
                  -> Either SrcError (Program, [SrcError])
        parseProg o = evalRLPC o . (lexCore >=> parseCoreProg)

        -- choose the appropriate model based on the compiler opts
        chooseEval = do
            ev <- view rlpcEvaluator
            pure $ case ev of
                EvaluatorGM  -> v GM.hdbgProg
                EvaluatorTIM -> v TIM.hdbgProg
            where v f p h = f p h *> pure ()

