-- | This module implements the server-side functionality for the administrator
-- panel in Dfterm3 web interface.
--

{-# LANGUAGE OverloadedStrings #-}

module Dfterm3.AdminPanel
    ( startAdminPanel )
    where

import Dfterm3.User
import Dfterm3.Logging

import qualified Happstack.Server.SimpleHTTP as H
import qualified Happstack.Server.FileServe as H
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as B64
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Control.Applicative ( (<$>) )
import Data.Word ( Word16 )

--import Text.Blaze ((!))
--import Text.Blaze.Html5.Strict
--import Text.Blaze.Html5.Strict.Attributes

-- | Starts the admin panel. This starts a HTTP server on the given port where
-- one can connect and then access stuff.
--
-- This function does not return. Use a `forkIO` if you want it asynchronous.
startAdminPanel :: UserSystem -> Word16 -> IO ()
startAdminPanel us port = do
    -- We need to create the socket ourselves to listen on 127.0.0.1
    s <- H.bindIPv4 "127.0.0.1" $ fromIntegral port
    H.simpleHTTPWithSocket s
                           H.nullConf { H.port = fromIntegral port }
                           (adminPart us)

adminPart :: UserSystem -> H.ServerPart H.Response
adminPart us = msum [ H.dir "admin" $ adminPanelRoot us ]

adminPanelRoot :: UserSystem -> H.ServerPart H.Response
adminPanelRoot us = msum [ H.dir "resources" $
                           H.serveDirectory H.DisableBrowsing []
                                            "./web-interface/resources"
                         , adminPanel us ]

adminPanel :: UserSystem -> H.ServerPart H.Response
adminPanel us =
    msum [ do H.method [ H.POST ]
              msum [ do H.decodeBody passwordCheckPolicy
                        password <- BU.fromString <$>
                                    (H.body $ H.look "password")
                        maybe_admin <- liftIO $ openAdmin password 1800 us
                        case maybe_admin of
                            Nothing -> do
                                host <- H.rqPeer <$> H.askRq
                                liftIO $ logNotice $
                                          "Failed administrator panel login \
                                          \from " ++ show host
                                loginScreen

                            Just admin -> do
                                host <- H.rqPeer <$> H.askRq
                                liftIO $ logNotice $
                                          "Administrator logged in from " ++
                                          show host
                                makeSessionId admin
                                adminPanelAuthenticated us
                   , loginScreen ]

         , do session_id <- H.readCookieValue "dfterm3_admin_session_id"
              validateSessionId session_id $ adminPanelAuthenticated us

         , loginScreen ]
  where
    loginScreen = H.serveFile (H.asContentType "text/html")
                       "./web-interface/admin-login.html"
    passwordCheckPolicy = H.defaultBodyPolicy "/tmp/" 0 10000 1000

validateSessionId :: String
                  -> H.ServerPart H.Response
                  -> H.ServerPart H.Response
validateSessionId _ _ = mzero

makeSessionId :: Admin -> H.ServerPart ()
makeSessionId admin = do
    H.addCookie H.Session (H.mkCookie "dfterm3_admin_session_id"
                                      (BU.toString . B64.encode $
                                       adminSessionID admin))
                          { H.httpOnly = True }

adminPanelAuthenticated :: UserSystem -> H.ServerPart H.Response
adminPanelAuthenticated us =
    H.serveFile (H.asContentType "text/html")
                 "./web-interface/admin.html"

