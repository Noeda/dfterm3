-- | This module implements the server-side functionality for the administrator
-- panel in Dfterm3 web interface.
--

{-# LANGUAGE OverloadedStrings #-}

module Dfterm3.AdminPanel
    ( startAdminPanel )
    where

import Dfterm3.Util ( whenJust )
import Dfterm3.User
import Dfterm3.Logging
import Dfterm3.GamePool
import Dfterm3.DwarfFortress ( enumerateRunningGames )
import Dfterm3.DwarfFortress.Types

import qualified Happstack.Server.SimpleHTTP as H
import qualified Happstack.Server.FileServe as H
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Base64 as B64
import Control.Monad
import Control.Lens
import Control.Monad.IO.Class ( liftIO )
import Control.Applicative ( (<$>) )
import Data.Word ( Word16 )

import Text.Blaze ((!))
import qualified Text.Blaze.Html5 as L
import qualified Text.Blaze.Html5.Attributes as A

-- | Starts the admin panel. This starts a HTTP server on the given port where
-- one can connect and then access stuff.
--
-- This function does not return. Use a `forkIO` if you want it asynchronous.
startAdminPanel :: GamePool -> UserSystem -> Word16 -> IO ()
startAdminPanel pool us port = do
    -- We need to create the socket ourselves to listen on 127.0.0.1
    s <- H.bindIPv4 "127.0.0.1" $ fromIntegral port
    H.simpleHTTPWithSocket s
                           H.nullConf { H.port = fromIntegral port }
                           (adminPart pool us)

adminPart :: GamePool -> UserSystem -> H.ServerPart H.Response
adminPart pool us = msum [ H.dir "admin" $ adminPanelRoot pool us
                         , do H.nullDir
                              H.movedPermanently ("admin/" :: T.Text) $
                                 H.toResponse ("Redirecting..." :: T.Text) ]

adminPanelRoot :: GamePool -> UserSystem -> H.ServerPart H.Response
adminPanelRoot pool us = msum [ H.dir "resources" $
                           H.serveDirectory H.DisableBrowsing []
                                            "./web-interface/resources"
                         , adminPanel pool us ]

adminPanel :: GamePool -> UserSystem -> H.ServerPart H.Response
adminPanel pool us =
    msum [ H.dir "admin_login" $ do
              H.method [ H.POST ]
              msum [ do H.decodeBody decodePolicy
                        password <- BU.fromString <$>
                                    H.body (H.look "password")
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
                                adminPanelAuthenticated admin pool us
                   , loginScreen ]

         , do session_id <- H.readCookieValue "dfterm3_admin_session_id"
              validateSessionId session_id us $ \admin ->
                  adminPanelAuthenticated admin pool us

         , loginScreen ]

loginScreen :: H.ServerPart H.Response
loginScreen = H.serveFile (H.asContentType "text/html")
                   "./web-interface/admin-login.html"

decodePolicy :: H.BodyPolicy
decodePolicy = H.defaultBodyPolicy "/tmp/" 0 10000 1000

validateSessionId :: String
                  -> UserSystem
                  -> (Admin -> H.ServerPart H.Response)
                  -> H.ServerPart H.Response
validateSessionId session_id us action = do
    let Right decoded_b64 = B64.decode (BU.fromString session_id)
    maybe_admin <- liftIO $ openAdminBySessionID decoded_b64 us
    case maybe_admin of
        Nothing -> loginScreen
        Just admin -> action admin

makeSessionId :: Admin -> H.ServerPart ()
makeSessionId admin =
    H.addCookie H.Session (H.mkCookie "dfterm3_admin_session_id"
                                      (BU.toString . B64.encode $
                                       adminSessionID admin))
                          { H.httpOnly = True }

first :: (a -> Bool) -> [a] -> Maybe a
first _ [] = Nothing
first predicate (x:xs)
    | predicate x = Just x
    | otherwise = first predicate xs

registerGameByExecutable :: String -> String -> GamePool -> UserSystem -> IO ()
registerGameByExecutable executable name pool us = do
    states <- enumerateRunningGames pool
    let maybe_target = first (\x -> x^.dfExecutable == executable)
                             (fmap _df states)
    whenJust maybe_target $ \target -> do
        result <- registerDwarfFortress executable
                                        ["--start-dfterm3-plugin"]
                                        (target^.dfWorkingDirectory)
                                        name
                                        us
        case result of
            New -> logNotice $
                   "Registered a Dwarf Fortress game with executable path " ++
                   "'" ++ executable ++ "', working directory '" ++
                   (target^.dfWorkingDirectory) ++ "' and name '" ++
                   "Dwarf Fortress'"
            Replaced -> logNotice $
                   "Modified a Dwarf Fortress game with executable path " ++
                   "'" ++ executable ++ "', working directory '" ++
                   (target^.dfWorkingDirectory) ++ "' and name '" ++
                   name ++ "'"

adminPanelAuthenticated :: Admin -> GamePool -> UserSystem
                        -> H.ServerPart H.Response
adminPanelAuthenticated admin pool us = msum
    [ H.dir "register_game" $ do
        H.method [ H.POST ]
        H.decodeBody decodePolicy
        executable <- blook "key"
        registerExecutableWithName executable
        adminPanelListings pool us Nothing

    , H.dir "change_password" $ do
        H.method [ H.POST ]
        H.decodeBody decodePolicy
        old_password <- BU.fromString <$> blook "old_password"
        password <- BU.fromString <$> blook "password"
        retype_password <- BU.fromString <$> blook "retype_password"

        if B.null password
          then adminPanelListings pool us (Just "Password cannot be empty.")
          else do

        maybe_admin <- liftIO $ openAdmin old_password 10 us
        case maybe_admin of
            Nothing -> adminPanelListings pool us (Just "Incorrect password.")
            Just admin -> do
                liftIO $ expireAdmin admin us
                if password /= retype_password
                  then adminPanelListings pool us
                                          (Just "Passwords don't match.")
                  else do liftIO $ setAdminPassword (Just password) us
                          adminPanelListings pool us
                                            (Just "Password set.")

    , modifyGameDecoding $ \executable -> do
        _ <- blook "modify"
        registerExecutableWithName executable
        panelListings

    , modifyGameDecoding $ \executable -> do
        _ <- blook "unregister"
        liftIO $ do
            unregisterDwarfFortress executable us
            logNotice $ "Unregistered a Dwarf Fortress by the executable name '"
                        ++ executable ++ "'."
        panelListings

    , H.dir "logout" $ do
        H.method [ H.POST ]
        liftIO $ expireAdmin admin us
        loginScreen

    , panelListings ]
  where
    registerExecutableWithName executable = do
        name <- blook "name"
        liftIO $ registerGameByExecutable executable name pool us

    panelListings = adminPanelListings pool us Nothing
    blook = H.body . H.look

    withPostDecoding action = do
        H.method [ H.POST ]
        H.decodeBody decodePolicy
        action

    withGameModifyDecoding action =
        withPostDecoding $ H.body (H.look "key") >>= action

    modifyGameDecoding action =
        H.dir "modify_game" $ withGameModifyDecoding action

adminPanelListings :: GamePool
                   -> UserSystem
                   -> Maybe T.Text
                   -> H.ServerPart H.Response
adminPanelListings pool us maybe_flash = do
    states <- liftIO $ enumerateRunningGames pool
    dfs <- liftIO $ listDwarfFortresses us

    let unregistered_games =
            filter (\x ->
                not $ any (\y -> y^.dfExecutable == x^.dfExecutable) dfs)
                   (fmap _df states)

    H.ok $ H.toResponse $ do
    L.html $ do
        L.head $ do
            L.title (L.toHtml ("Dfterm3 Admin Panel" :: String))
            L.meta ! A.charset "utf-8"
            L.link ! A.href "resources/interface.css" ! A.rel "stylesheet" !
                     A.type_ "text/css" ! A.title "Interface style"
        L.body $
            L.div ! A.class_ "admin_content" $ do

                whenJust maybe_flash $ \flash ->
                    L.div ! A.class_ "admin_flash" $ L.p (L.toHtml flash)

                L.form ! A.action "logout" !
                         A.method "post" $
                    L.input ! A.type_ "submit" ! A.value "Logout"

                L.div ! A.class_ "admin_password_form" $ do
                    L.form ! A.action "change_password" !
                             A.method "post" $ do
                        L.h3 "Change administrator password:"
                        L.label "Old password: "
                        L.br
                        L.input ! A.name "old_password" ! A.type_ "password"
                        L.br
                        L.label "Password: "
                        L.br
                        L.input ! A.name "password" ! A.type_ "password"
                        L.br
                        L.label "Retype password: "
                        L.br
                        L.input ! A.name "retype_password" ! A.type_ "password"
                        L.br
                        L.input ! A.type_ "submit" ! A.value "Change password"

                unless (null unregistered_games) $ do

                    L.div ! A.class_ "admin_title_unregistered" $
                        L.h3 "Unconfigured, running Dwarf Fortress games"
                    L.br
                    L.ul $
                        forM_ unregistered_games $ \name ->
                            L.li $
                                L.form ! A.action "register_game" !
                                         A.method "post" $ do
                                    L.input ! A.type_ "hidden" !
                                              A.name "key" !
                                              A.value (L.toValue
                                                  (name ^. dfExecutable))
                                    L.input ! A.type_ "submit" ! A.value "Add"
                                    L.toHtml (name ^. dfExecutable)
                                    L.span ! A.class_ "game_name" $
                                        L.input ! A.type_ "text" !
                                                  A.name "name" !
                                                  A.value "Dwarf Fortress"

                if null dfs
                  then L.h3 "No registered Dwarf Fortresses"
                  else
                    L.div ! A.class_ "admin_title_registered" $
                        L.h3 "Registered Dwarf Fortress games"
                L.br

                L.ul $
                    forM_ dfs $ \df ->
                        L.li $
                            L.form ! A.action "modify_game" !
                                     A.method "post" $ do
                                L.input ! A.type_ "hidden" !
                                          A.name "key" !
                                          A.value (L.toValue
                                              (df ^. dfExecutable))
                                L.input ! A.type_ "submit" ! A.name "modify" !
                                          A.value "Modify"
                                L.input ! A.type_ "submit" ! A.name "unregister" !
                                          A.value "Unregister"
                                L.toHtml (df ^. dfExecutable)
                                L.span ! A.class_ "game_name" $
                                    L.input ! A.type_ "text" !
                                              A.name "name" !
                                              A.value
                                                  (L.toValue $ df ^. dfName)


