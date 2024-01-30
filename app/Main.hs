{-# LANGUAGE BlockArguments, LambdaCase #-}
module Main where
----------------------------------------------------------------------------------
import Compiler.RLPC
import Control.Exception
import Options.Applicative      hiding (ParseError)
import Control.Monad
import Control.Monad.Reader
import Data.HashSet             qualified as S
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Text.IO             qualified as TIO
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
    <*> option languageReader
        (  long "language"
        )
    <*> some (argument str $ metavar "FILES...")
    where
        infixr 9 #
        f # x = f x

languageReader :: ReadM Language
languageReader = maybeReader $ \case
    "rlp"   -> Just LanguageRlp
    "core"  -> Just LanguageCore

debugFlagReader :: ReadM DebugFlag
debugFlagReader = maybeReader $ Just

evaluatorReader :: ReadM Evaluator
evaluatorReader = maybeReader $ \case
    "gm"  -> Just EvaluatorGM
    "ti"  -> Just EvaluatorTI
    _     -> Nothing

mmany :: (Alternative f, Monoid m) => f m -> f m
mmany v = liftA2 (<>) v (mmany v)

----------------------------------------------------------------------------------

main :: IO ()
main = do
    opts <- execParser optParser
    void $ evalRLPCIO opts driver

driver :: RLPCIO ()
driver = sequence_
    [ dshowFlags
    , ddumpAST
    , ddumpEval
    ]

dshowFlags :: RLPCIO ()
dshowFlags = whenDFlag "dump-flags" do
    ask >>= liftIO . print

ddumpAST :: RLPCIO ()
ddumpAST = whenDFlag "dump-ast" $ forFiles_ \o f -> do
    liftIO $ withFile f ReadMode $ \h -> do
        s <- TIO.hGetContents h
        case parseProg o s of
            Right (a,_) -> hPutStrLn stderr $ show a
            Left e -> error "todo errors lol"

ddumpEval :: RLPCIO ()
ddumpEval = whenDFlag "dump-eval" do
    fs <- view rlpcInputFiles
    forM_ fs $ \f -> liftIO (TIO.readFile f) >>= doProg

    where
        doProg :: Text -> RLPCIO ()
        doProg = undefined
        -- doProg s = ask >>= \o -> case parseProg o s of
        --     -- TODO: error handling
        --     Left e -> addFatal . CompilerError $ show e
        --     Right (a,_) -> do
        --         log <- view rlpcLogFile
        --         dumpEval <- chooseEval
        --         case log of
        --             Just f  -> liftIO $ withFile f WriteMode $ dumpEval a
        --             Nothing -> liftIO $ dumpEval a stderr

        -- choose the appropriate model based on the compiler opts
        -- chooseEval = do
        --     ev <- view rlpcEvaluator
        --     pure $ case ev of
        --         EvaluatorGM  -> v GM.hdbgProg
        --         EvaluatorTI -> v TI.hdbgProg
        --     where v f p h = f p h *> pure ()

parseProg :: RLPCOptions
          -> Text
          -> (Maybe Program', [MsgEnvelope RlpcError])
parseProg o = evalRLPC o . (lexCore >=> parseCoreProg)

forFiles_ :: (Monad m)
          => (RLPCOptions -> FilePath -> RLPCT m a)
          -> RLPCT m ()
forFiles_ k = do
    fs <- view rlpcInputFiles
    o <- ask
    forM_ fs (k o)

