{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
----------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Exception
import Options.Applicative      hiding (ParseError)
import Control.Monad
import Control.Monad.Reader
import Data.HashSet             qualified as S
import System.IO
import System.Exit              (exitSuccess)
import Core
import TI
import GM
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
        <> help "output dumps to FILE. stderr is used if unset"
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
        <> metavar "gm|ti"
        <> value EvaluatorGM
        <> help "the intermediate layer used to model evaluation"
        )
    <*> option auto
        (  long "heap-trigger"
        <> metavar "INT"
        <> help "the number of nodes allowed on the heap before\
                \triggering the garbage collector"
        <> value 50
        )
    <*> some (argument str $ metavar "FILES...")
    where
        infixr 9 #
        f # x = f x

evaluatorReader :: ReadM Evaluator
evaluatorReader = maybeReader $ \case
    "gm"  -> Just EvaluatorGM
    "tim" -> Just EvaluatorTI
    _     -> Nothing

mmany :: (Alternative f, Monoid m) => f m -> f m
mmany v = liftA2 (<>) v (mmany v)

debugFlagReader :: ReadM DebugFlag
debugFlagReader = maybeReader $ \case
    "dump-eval"  -> Just DDumpEval
    "dump-opts"  -> Just DDumpOpts
    "dump-ast"   -> Just DDumpAST
    _            -> Nothing

----------------------------------------------------------------------------------

-- temp
data CompilerError = CompilerError String
    deriving Show

instance Exception CompilerError

main :: IO ()
main = do
    opts <- execParser optParser
    (_, es) <- evalRLPCIO opts driver
    forM_ es $ \ (CompilerError e) -> print $ "warning: " <> e
    pure ()

driver :: RLPCIO CompilerError ()
driver = sequence_
    [ dshowFlags
    , ddumpAST
    , ddumpEval
    ]

dshowFlags :: RLPCIO CompilerError ()
dshowFlags = whenFlag flagDDumpOpts do
    ask >>= liftIO . print

ddumpAST :: RLPCIO CompilerError ()
ddumpAST = whenFlag flagDDumpAST $ forFiles_ \o f -> do
    liftIO $ withFile f ReadMode $ \h -> do
        s <- hGetContents h
        case parseProg o s of
            Right (a,_) -> hPutStrLn stderr $ show a
            Left e -> error "todo errors lol"

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

        -- choose the appropriate model based on the compiler opts
        chooseEval = do
            ev <- view rlpcEvaluator
            pure $ case ev of
                EvaluatorGM  -> v GM.hdbgProg
                EvaluatorTI -> v TI.hdbgProg
            where v f p h = f p h *> pure ()

parseProg :: RLPCOptions
          -> String
          -> Either SrcError (Program', [SrcError])
parseProg o = evalRLPC o . (lexCore >=> parseCoreProg)

forFiles_ :: (Monad m)
          => (RLPCOptions -> FilePath -> RLPCT e m a)
          -> RLPCT e m ()
forFiles_ k = do
    fs <- view rlpcInputFiles
    o <- ask
    forM_ fs (k o)

