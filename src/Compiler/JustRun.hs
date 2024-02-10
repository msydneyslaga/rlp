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
    , justTypeCheckCore
    )
    where
----------------------------------------------------------------------------------
import Core.Lex
import Core.Parse
import Core.HindleyMilner
import Core.Syntax                      (Program')
import Compiler.RLPC
import Control.Arrow                    ((>>>))
import Control.Monad                    ((>=>))
import Control.Comonad
import Control.Lens
import Data.Text                        qualified as T
import Data.Function                    ((&))
import GM
----------------------------------------------------------------------------------

justLexCore :: String -> Either [MsgEnvelope RlpcError] [CoreToken]
justLexCore s = lexCoreR (T.pack s)
              & mapped . each %~ extract
              & rlpcToEither

justParseCore :: String -> Either [MsgEnvelope RlpcError] Program'
justParseCore s = parse (T.pack s)
                & rlpcToEither
    where parse = lexCoreR >=> parseCoreProgR

justTypeCheckCore :: String -> Either [MsgEnvelope RlpcError] Program'
justTypeCheckCore s = typechk (T.pack s)
                    & rlpcToEither
    where typechk = lexCoreR >=> parseCoreProgR >=> checkCoreProgR

rlpcToEither :: RLPC a -> Either [MsgEnvelope RlpcError] a
rlpcToEither r = case evalRLPC def r of
    (Just a,   _) -> Right a
    (Nothing, es) -> Left es

