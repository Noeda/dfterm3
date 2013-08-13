-- | A very simple channel system.
--
-- You register a channel, and then you can just send chat messages to it, as
-- long as you provide a user name and the contents of the message.
--
-- Listeners register to the channel and they can then listen on incoming
-- messages.

{-# LANGUAGE DeriveDataTypeable #-}

module Dfterm3.ChatChannel
    ( registerChannel
    , registerAsListener
    , chat
    , listen
    , Channel()
    , ChannelListener() )
    where

import Data.Typeable
import Control.Concurrent.STM
import qualified Data.Text as T

-- | A channel type. You can send messages to it.
newtype Channel = Channel (TChan (T.Text, T.Text))
                  deriving ( Typeable, Eq )
-- | A channel listener. It receives messages sent to a `Channel`.
newtype ChannelListener = ChannelListener (TChan (T.Text, T.Text))
                          deriving ( Typeable, Eq )

registerChannel :: IO Channel
registerChannel = do
    chan <- newBroadcastTChanIO
    return $ Channel chan

-- | Sends a message to a channel.
chat :: T.Text -> T.Text -> Channel -> IO ()
chat user_name msg (Channel chan) =
    atomically $ writeTChan chan (user_name, msg)

-- | Register as a listener on some channel.
registerAsListener :: Channel -> IO ChannelListener
registerAsListener (Channel chan) =
    ChannelListener `fmap` atomically (dupTChan chan)

-- | Block until someone sends a message to the channel.
listen :: ChannelListener -> IO (T.Text, T.Text)
listen (ChannelListener chan) = atomically $ readTChan chan

