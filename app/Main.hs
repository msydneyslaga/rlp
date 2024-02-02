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
import Data.List
import System.IO
import System.Exit              (exitSuccess)
import Core
import TI
import GM
import Lens.Micro.Mtl

import CoreDriver               qualified
import RlpDriver                qualified
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
        <> help "pass debug flags"
        <> metavar "DEBUG FLAG"
        )
    {- -f -}
    <*> fmap S.fromList # many # option compilerFlagReader
        (  short 'f'
        <> help "pass compilation flags"
        <> metavar "COMPILATION FLAG"
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
        <> short 'x'
        <> metavar "rlp|core"
        <> help "the language to be compiled -- see README"
        )
    <*> some (argument str $ metavar "FILES...")
    where
        infixr 9 #
        f # x = f x

languageReader :: ReadM Language
languageReader = maybeReader $ \case
    "rlp"   -> Just LanguageRlp
    "core"  -> Just LanguageCore
    _       -> Nothing

debugFlagReader :: ReadM DebugFlag
debugFlagReader = str

compilerFlagReader :: ReadM CompilerFlag
compilerFlagReader = str

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
driver = view rlpcLanguage >>= \case
    LanguageCore -> CoreDriver.driver
    LanguageRlp  -> RlpDriver.driver

