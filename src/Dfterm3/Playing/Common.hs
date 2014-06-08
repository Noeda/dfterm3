{-# LANGUAGE DeriveGeneric, LambdaCase, RecordWildCards #-}

module Dfterm3.Playing.Common
    ( STCMessage()
    , CTSMessage()
    , runPlayingSession )
    where

import Dfterm3.Prelude
import Dfterm3.User
import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Game.DwarfFortress
import Data.Aeson
import GHC.Generics
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Strict
import qualified Data.Text as T
import Control.Concurrent
import System.Timeout

data CTSMessage =
    Unsubscribe
  | Chat !T.Text
  | DoInput !KeyDirection !Input
  | Subscribe !Int
  | Authenticate { name :: T.Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Generic )

data STCMessage = LoginSuccessful
                | LoginFailed
                deriving ( Eq, Ord, Show, Read, Typeable, Generic )

instance FromJSON CTSMessage
instance ToJSON CTSMessage
instance FromJSON STCMessage
instance ToJSON STCMessage

newtype Session a =
    Session { runSession ::
              RWST SessionEnv () SessionVar IO a }
    deriving ( Monad, MonadIO, Applicative, Functor, Typeable )

data SessionEnv = SessionEnv
    { sender :: STCMessage -> IO ()
    , receiver :: IO CTSMessage
    , killer :: IO ()
    , storage :: Storage
    , user :: User }

data SessionVar = SessionVar

recv :: Session CTSMessage
recv = Session $ do
    SessionEnv { receiver = receiver } <- ask
    liftIO receiver

send :: STCMessage -> Session ()
send msg = Session $ do
    SessionEnv { sender = sender } <- ask
    liftIO $ sender msg

die :: Session a
die = Session $ do
    SessionEnv { killer = killer } <- ask
    liftIO $ do
        killer
        -- if we are still alive, make sure we die too
        myThreadId >>= killThread
        undefined -- never reached

-- Do something in a session within a time limit. If the limit is reached, kill
-- the session.
--
-- Time is in microseconds.
timeoutOrDie :: Int -> Session a -> Session a
timeoutOrDie useconds (Session rws) = Session $ do
    env <- ask
    st <- get
    maybe_result <- liftIO $ timeout useconds $ runRWST rws env st
    case maybe_result of
        Nothing -> runSession die
        Just (result, new_st, _) -> do
            put new_st
            return result

runPlayingSession :: (STCMessage -> IO ())
                  -> IO CTSMessage
                  -> IO ()
                  -> Storage
                  -> IO ()
runPlayingSession sender receiver killer ps = do
    usr <- liftIO $ newGuestUser ps
    void $ execRWST session SessionEnv
                            { sender = sender
                            , receiver = receiver
                            , killer = killer
                            , storage = ps
                            , user = usr }
                            SessionVar {}
  where
    session = runSession $ do
        -- if handshake is not complete within 10 seconds, cut the connection.
        timeoutOrDie 10000000 handshake
        forever $ void recv

handshake :: Session ()
handshake =
    recv >>= \case
        Authenticate {..} -> do
            usr <- Session $ user <$> ask
            st <- Session $ storage <$> ask
            x <- liftIO $ loginUser usr name st
            if x
              then send LoginSuccessful
              else send LoginFailed >> handshake
        _ -> handshake

