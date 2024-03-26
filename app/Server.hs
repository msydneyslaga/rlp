{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Server
    ( server
    )
    where
--------------------------------------------------------------------------------
import GHC.Generics                 (Generic, Generically(..))
import Data.Text.Encoding           qualified as T
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.Text.IO                 qualified as T
import Data.Pretty                  hiding (annotate)
import Data.Aeson
import Data.Function
import Control.Arrow
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Network.WebSockets           qualified as WS
import Control.Exception
import GHC.IO
import Control.Lens                 hiding ((.=))

import Control.Comonad
import Data.Functor.Foldable

import Compiler.RLPC

import Misc.CofreeF
import Rlp.AltSyntax
import Rlp.HindleyMilner
import Rlp.AltParse
--------------------------------------------------------------------------------

server :: IO ()
server = do
    T.putStrLn "rlpc server started at 127.0.0.1:9002"
    WS.runServer "127.0.0.1" 9002 application

application :: WS.ServerApp
application pending = do
    WS.acceptRequest pending >>= talk

data Command = Annotate Text
             | PartiallyAnnotate Text
    deriving Show

instance FromJSON Command where
    parseJSON = withObject "command object" $ \v -> do
        cmd :: Text <- v .: "command"
        case cmd of
            "annotate"           -> Annotate <$> v .: "source"
            "partially-annotate" -> PartiallyAnnotate <$> v .: "source"
            _          -> empty

data Response = Annotated Value
              | PartiallyAnnotated Value
    deriving (Generic)
    deriving (ToJSON)
        via Generically Response

talk :: WS.Connection -> IO ()
talk conn = (`catchAny` print) . forever $ do
    msg <- WS.receiveData @Text conn
    T.putStrLn $ "received: " <> msg
    doCommand conn `traverse` decodeStrictText msg

doCommand :: WS.Connection -> Command -> IO ()
doCommand conn c = do
    putStr "sending: "
    let r = encode . respond $ c
    print r
    WS.sendTextData conn r

respond :: Command -> Response
respond (Annotate s)
    = s & (parseRlpProgR >=> typeCheckRlpProgR)
        & fmap (\p -> p ^.. funDs
           <&> serialiseSc)
        & runRLPCJsonDef
        & Annotated

showPartialAnn = undefined

funDs :: Traversal' (Program b a) (b, [Pat b], a)
funDs = programDecls . each . _FunD

serialiseSc :: (PsName, [Pat PsName], Cofree (RlpExprF PsName) (Type PsName))
            -> Value
serialiseSc (n,as,e) = object
    [ "name" .= n
    , "args" .= as
    , "body" .= let rootType = extract e
                in serialiseAnnotated (e <&> prettyVars rootType) ]

serialiseAnnotated :: Cofree (RlpExprF PsName) (Type PsName)
                   -> Value
serialiseAnnotated = cata \case
    t :<$ e -> object [ "e" .= e, "type" .= rout @Text t ]

runRLPCJsonWithDef :: (a -> Value) -> RLPC a -> Value
runRLPCJsonWithDef f = runRLPCJsonWith f def

runRLPCJsonDef :: (ToJSON a) => RLPC a -> Value
runRLPCJsonDef = runRLPCJsonWith toJSON def

runRLPCJsonWith :: (a -> Value) -> RLPCOptions -> RLPC a -> Value
runRLPCJsonWith f o r = object
    [ "errors" .= es
    , "result" .= (f <$> ma) ]
  where (ma,es) = evalRLPC o r

