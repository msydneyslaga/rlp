{-|
Module      : Compiler.JustRun
Description : No-BS, high-level wrappers for major pipeline pieces.

A collection of wrapper functions to demo processes such as lexing, parsing,
type-checking, and evaluation. This module intends to export "no-BS" functions
that use Prelude types such as @Either@ and @String@ rather than more complex
types such as @RLPC@ or @Text@.
-}
module Compiler.JustRun
    ( justLexCore
    , justParseCore
    , justParseRlp
    , justTypeCheckCore
    , justHdbg
    , makeItPretty, makeItPretty'
    )
    where
----------------------------------------------------------------------------------
import Core.Lex
import Core.Parse
import Core.HindleyMilner
import Core.Syntax
import Compiler.RLPC
import Control.Arrow                    ((>>>))
import Control.Monad                    ((>=>), void)
import Control.Comonad
import Control.Lens
import Data.Text                        qualified as T
import Data.Function                    ((&))
import System.IO
import GM
import Rlp2Core
import Data.Pretty

import Rlp.AltParse
import Rlp.AltSyntax                    qualified as Rlp
----------------------------------------------------------------------------------

justHdbg :: String -> IO GmState
justHdbg = undefined
-- justHdbg s = do
--     p <- evalRLPCIO def (parseRlpProgR >=> desugarRlpProgR $ T.pack s)
--     withFile "/tmp/t.log" WriteMode $ hdbgProg p

justLexCore :: String -> Either [MsgEnvelope RlpcError] [CoreToken]
justLexCore s = lexCoreR (T.pack s)
              & mapped . each %~ extract
              & rlpcToEither

justParseCore :: String -> Either [MsgEnvelope RlpcError] (Program Var)
justParseCore s = parse (T.pack s)
                & rlpcToEither
    where parse = lexCoreR @Identity >=> parseCoreProgR

justParseRlp :: String
             -> Either [MsgEnvelope RlpcError]
                       (Rlp.Program Name (Rlp.RlpExpr Name))
justParseRlp s = parse (T.pack s)
               & rlpcToEither
    where parse = parseRlpProgR @Identity

justTypeCheckCore :: String -> Either [MsgEnvelope RlpcError] Program'
justTypeCheckCore s = typechk (T.pack s)
                    & rlpcToEither
    where typechk = lexCoreR >=> parseCoreProgR >=> checkCoreProgR

makeItPretty :: (Pretty a) => Either e a -> Either e Doc
makeItPretty = fmap pretty

makeItPretty' :: (Pretty (WithTerseBinds a)) => Either e a -> Either e Doc
makeItPretty' = fmap (pretty . WithTerseBinds)

rlpcToEither :: RLPC a -> Either [MsgEnvelope RlpcError] a
rlpcToEither r = case evalRLPC def r of
    (Just a,   _) -> Right a
    (Nothing, es) -> Left es

