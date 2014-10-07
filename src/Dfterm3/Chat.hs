module Dfterm3.Chat
    ( newChatRoom
    , joinChatRoom
    , ChatText
    , ChatRoomHandle
    , ChatMessage(..)
    , ChatEvent(..)
    , ChatRoom() )
    where

import Dfterm3.Prelude
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text as T

type ChatText = T.Text

data ChatMessage = ChatMessage !T.Text !T.Text
                   deriving ( Eq, Ord, Show, Read, Typeable )

data ChatEvent
    = Joined !T.Text
    | Parted !T.Text
    | Messaged !ChatMessage
    deriving ( Eq, Ord, Show, Read, Typeable )

data ChatRoom = MkChatRoom
    { _broadcast :: !(TChan ChatEvent) }
    deriving ( Eq, Typeable )

type ChatRoomHandle = ChatMessage -> STM ()

newChatRoom :: IO ChatRoom
newChatRoom = do
    chan <- newBroadcastTChanIO
    return MkChatRoom { _broadcast = chan }

joinChatRoom :: ChatRoom -> IO (ChatMessage -> STM (), TChan ChatEvent)
joinChatRoom chatroom = do
    chan <- atomically $ dupTChan (_broadcast chatroom)
    return ( (\msg -> writeTChan (_broadcast chatroom) (Messaged msg))
           , chan )

