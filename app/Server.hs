{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
module Server
    ( server
    )
    where
--------------------------------------------------------------------------------
import GHC.Generics                 (Generic, Generically(..))
import Data.Text                    (Text)
import Data.Text                    qualified as T
import Data.Text.IO                 qualified as T
import Data.Pretty
import Data.Aeson
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Network.WebSockets           qualified as WS

import Data.Functor.Foldable

import Misc.CofreeF
import Rlp.AltSyntax
import Rlp.HindleyMilner
import Rlp.AltParse
--------------------------------------------------------------------------------

server :: IO ()
server = do
    WS.runServer "127.0.0.1" 9002 application

application :: WS.ServerApp
application pending =
    WS.acceptRequest pending >>= talk

newtype Command = Annotate Text

instance FromJSON Command where
    parseJSON = withObject "command object" $ \v -> do
        cmd :: Text <- v .: "command"
        case cmd of
            "annotate" -> Annotate <$> v .: "data"
            _          -> empty

data Response = Annotated Value
              | PartiallyAnnotated Value
    deriving (Generic)
    deriving (ToJSON)
        via Generically Response

talk :: WS.Connection -> IO ()
talk conn = forever $ do
    msg <- WS.receiveData @Text conn
    T.putStrLn $ "received: " <> msg
    case decodeStrictText msg of
        Just c -> doCommand c
        Nothing -> WS.sendTextData @Text conn "\"error while parsing json\""

doCommand :: Command -> IO ()
doCommand (Annotate s) = undefined

parse = undefined

serialisedAnnotated :: Cofree (RlpExprF PsName) (Type PsName)
                    -> Value
serialisedAnnotated = cata \case
    t :<$ e -> object [ "e" .= e, "type" .= rout @Text t ]

