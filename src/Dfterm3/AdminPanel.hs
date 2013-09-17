-- | An administrator panel served through HTTP.
--

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Dfterm3.AdminPanel
    ( runAdminPanel )
    where

import Dfterm3.Admin
import Dfterm3.Dfterm3State
import Dfterm3.Logging

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class ( liftIO )
import Data.Typeable ( Typeable )
import Data.Word
import Data.Monoid
import Network

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T

import Text.Blaze ((!))
import qualified Happstack.Server.SimpleHTTP as H
import qualified Happstack.Server.FileServe as H
import qualified Text.Blaze.Html5 as L
import qualified Text.Blaze.Html5.Attributes as A

-- | Runs an administrator panel. It runs until the Haskell process ends. In
-- other words, this does not return.
runAdminPanel :: Word16        -- ^ Port where to run.
              -> Storage       -- ^ Handle to persistent storage.
              -> IO ()
runAdminPanel port storage = do
    -- We need to create the socket ourselves to listen on 127.0.0.1
    s <- H.bindIPv4 "127.0.0.1" $ fromIntegral port
    H.simpleHTTPWithSocket s
                           H.nullConf { H.port = fromIntegral port }
                           (adminPart storage)

adminPart :: Storage -> H.ServerPart H.Response
adminPart storage = msum [ H.dir "admin" $ adminPanelRoot storage
                         , do H.nullDir
                              H.movedPermanently ("admin/" :: T.Text) $
                                 H.toResponse ("Redirecting..." :: T.Text) ]

adminPanelRoot :: Storage -> H.ServerPart H.Response
adminPanelRoot storage = msum [ H.dir "resources" $
                                H.serveDirectory H.DisableBrowsing []
                                                 "./web-interface/resources"
                              , adminPanel storage ]

adminPanel :: Storage -> H.ServerPart H.Response
adminPanel ps =
    msum [ H.dir "admin_login" $ do
              H.method [ H.POST ]
              msum [ do H.decodeBody decodePolicy
                        password <- BU.fromString <$>
                                    H.body (H.look "password")
                        maybe_sid <- liftIO $
                            newSessionByPassword password 1800 ps
                        case maybe_sid of
                            Nothing -> do
                                host <- H.rqPeer <$> H.askRq
                                liftIO $ logNotice $
                                          "Failed administrator panel login \
                                          \from " ++ show host
                                loginScreen

                            Just sid -> do
                                host <- H.rqPeer <$> H.askRq
                                liftIO $ logNotice $
                                          "Administrator logged in from " ++
                                          show host
                                setSessionIDCookie sid
                                showAdminPanelAuthenticated

                            _ -> mzero
                   , loginScreen ]

         , do Right session_id <-
                  B64.decode . BU.fromString <$>
                  H.readCookieValue "dfterm3_admin_session_id"
              liftIO $ logInfo (show session_id)
              mustHaveValidSessionID (byteStringToSessionID session_id) ps
              showAdminPanelAuthenticated

         , loginScreen ]
  where
    showAdminPanelAuthenticated = adminPanelAuthenticated ps

loginScreen :: H.ServerPart H.Response
loginScreen = H.serveFile (H.asContentType "text/html")
                   "./web-interface/admin-login.html"

decodePolicy :: H.BodyPolicy
decodePolicy = H.defaultBodyPolicy "/tmp/" 0 10000 1000

setSessionIDCookie :: SessionID -> H.ServerPart ()
setSessionIDCookie sid =
    H.addCookie H.Session (H.mkCookie "dfterm3_admin_session_id"
                                      (BU.toString . B64.encode $
                                       sessionIDToByteString sid))
                          { H.httpOnly = True }

mustHaveValidSessionID :: SessionID -> Storage -> H.ServerPart ()
mustHaveValidSessionID sid ps = do
    is_valid <- liftIO $ isValidSessionID sid ps
    unless is_valid mzero

data FlashMsg = Failure !T.Text | Success !T.Text
                deriving ( Eq, Ord, Show, Read, Typeable )

adminPanelAuthenticated :: Storage -> H.ServerPart H.Response
adminPanelAuthenticated ps = msum [
      -- The part that can change passwords
      H.dir "change_password" (changePasswordPart ps)
    ]

changePasswordPart :: Storage -> H.ServerPart H.Response
changePasswordPart ps = do
    H.method [ H.POST ]
    H.decodeBody decodePolicy
    old_password <- BU.fromString <$> blook "old_password"
    password <- BU.fromString <$> blook "password"
    retype_password <- BU.fromString <$> blook "retype_password"

    if B.null password
      then failure "Password cannot be empty."
      else do

    if password /= retype_password
      then failure "Passwords do not match."
      else do

    results <- liftIO $ changePassword old_password password ps
    if results
      then failure "Incorrect password."
      else success "Password changed."
  where
    blook = H.body . H.look

    failure = gameListings ps . Failure
    success = gameListings ps . Success

gameListings :: Storage -> FlashMsg -> H.ServerPart H.Response
gameListings _ msg = H.ok $ H.toResponse (show msg)

