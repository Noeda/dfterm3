module Dfterm3.Server.WebSocket
    ( runWebSocket )
    where

import Dfterm3.Dfterm3State
import Dfterm3.Server.Types
import qualified Dfterm3.Server.Logic as DSL

import Network.WebSockets
import Pipes

import Data.Typeable
import Data.Word
import Data.Aeson
import Control.Applicative
import Control.Monad

import qualified Data.Text as T

runWebSocket :: Word16 -> Storage -> IO ()
runWebSocket port storage =
    runServer "0.0.0.0" (fromIntegral port) $ \pending_conn -> do
        conn <- acceptRequest pending_conn
        client conn storage

client :: Connection -> Storage -> IO ()
client conn storage =
    runEffect $ receiver >->
                DSL.client storage sender
  where
    sender stc = sendDataMessage conn (Text $ encode stc)
    receiver = forever $ do
        Text msg <- liftIO $ receiveDataMessage conn
        let Just decoded = decode msg
        yield decoded

