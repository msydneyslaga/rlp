{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
----------------------------------------------------------------------------------
import Compiler.RLPC
import Compiler.RlpcError
import Control.Exception
import Options.Applicative      hiding (ParseError)
import Control.Monad
import Control.Monad.Reader
import Data.HashSet             qualified as S
import Data.Text                (Text)
import Data.Text                qualified as T
import Data.Text.IO             qualified as TIO
import Data.List
import Data.Maybe               (listToMaybe)
import System.IO
import System.Exit              (exitSuccess)
import Core
import TI
import GM
import Control.Lens.Combinators hiding (argument)

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
    <*> optional # option languageReader
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
    "rl"    -> Just LanguageRlp
    "cr"    -> Just LanguageCore
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
    void $ evalRLPCIO opts dispatch

dispatch :: RLPCIO ()
dispatch = getLang >>= \case
    Just LanguageCore -> CoreDriver.driver
    Just LanguageRlp  -> RlpDriver.driver
    Nothing           -> addFatal err
        where
            -- TODO: why didn't i make the srcspan optional LOL
            err = errorMsg (SrcSpan 0 0 0 0) $ Text
                [ "Could not determine source language from filetype."
                , "Possible Solutions:\n\
                  \  Suffix the file with `.cr' for Core, or `.rl' for rl'\n\
                  \  Specify a language with `rlpc -x core' or `rlpc -x rlp'"
                ]
  where
    getLang = liftA2 (<|>)
        (view rlpcLanguage)
        -- TODO: we only check the first file lol
        ((listToMaybe >=> inferLanguage) <$> view rlpcInputFiles)


driver :: RLPCIO ()
driver = undefined

inferLanguage :: FilePath -> Maybe Language
inferLanguage fp
    | ".rl" `isSuffixOf` fp     = Just LanguageRlp
    | ".cr" `isSuffixOf` fp     = Just LanguageCore
    | otherwise                 = Nothing

