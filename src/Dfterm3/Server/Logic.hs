{-# LANGUAGE RecordWildCards, OverloadedStrings #-}

module Dfterm3.Server.Logic
    ( client )
    where

import Dfterm3.UserAccounting
import Dfterm3.Dfterm3State
import Dfterm3.Server.Types
import Data.Maybe
import Data.Semigroup
import Control.Monad.Reader
import Control.Applicative
import Pipes hiding ( yield, await ) -- sneaky sneaky
import qualified Pipes as PC

type Client a = ReaderT (Storage, ServerToClient -> IO ())
                        (Consumer ClientToServer IO)
                        a

client :: Storage
       -> (ServerToClient -> IO ())
       -> Consumer ClientToServer IO ()
client storage sender = flip runReaderT (storage, sender) $ do
    yield handshake

    -- login
    doLogin

getStorage :: Client Storage
getStorage = fst <$> ask

doLogin :: Client ()
doLogin = do
    Login {..} <- await
    if identity == Guest
      then do allow_guests <- getStorage >>= liftIO . areGuestsEnabled
              if not allow_guests
                then do yield (LoginAck
                                 { status = False
                                 , notice = "Guest logins are disabled." })
                        doLogin
                else yield (LoginAck
                              { status = True
                              , notice = "" })
      else case identity of
               User username -> do
                   if isNothing password
                     then do yield (LoginAck
                                      { status = False
                                      , notice = "Password required." })
                             doLogin
                     else do valid <-
                                 getStorage >>= liftIO .
                                 isValidLogin username (fromJust password)
                             if valid
                               then yield (LoginAck
                                             { status = True
                                             , notice = "" })
                               else do yield (LoginAck
                                              { status = False
                                              , notice = "Invalid username " <>
                                                         "or password." })
                                       doLogin
               _ -> error "impossible"

-- Fake pipes. Maybe no one notices they are not quite real.
yield :: ServerToClient -> Client ()
yield msg = do
    sender <- snd <$> ask
    liftIO $ sender msg

await :: Client ClientToServer
await = lift $ PC.await

