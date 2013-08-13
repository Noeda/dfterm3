-- This module stores information that is lost when Dfterm3 server is shut
-- down.
--
-- Contrast to the `Dfterm.User` that deals with persistent information.
--

{-# LANGUAGE ViewPatterns #-}

module Dfterm3.UserVolatile
    ( UserVolatile()
    , User()
    , userName
    , newUserVolatile
    , login
    , logout )
    where

import Dfterm3.Logging
import Dfterm3.Util
import System.IO.Unsafe ( unsafePerformIO )
import Data.IORef
import Control.Applicative
import Control.Concurrent
import qualified Data.Text as T
import qualified Data.Set as S

newtype UserVolatile = UserVolatile (MVar UserVolatileInsides)

data UserVolatileInsides =
    UserVolatileInsides { _loggedInUsers :: S.Set T.Text }

data User = User (IORef T.Text)

-- | The IORef is never written into. Therefore this is safe usage of
-- `unsafePerformIO`.
userName :: User -> T.Text
userName (User name_ref) = unsafePerformIO $ readIORef name_ref

-- | Creates a new user volatile state.
newUserVolatile :: IO UserVolatile
newUserVolatile =
    UserVolatile <$>
        newMVar UserVolatileInsides { _loggedInUsers = S.empty }

login :: T.Text -> UserVolatile -> IO (Maybe User)
login (T.null -> True) _ = return Nothing
login name uv@(UserVolatile insides) =
    modifyMVar insides $ \u@(UserVolatileInsides loggings) -> do

        -- TODO: include IP-address in the logging messages here.
        ref <- newFinalizableIORef name $ do
            logoutRaw name uv
            logInfo $ "Logged out the user '" ++ T.unpack name ++ "'."
        logInfo $ "Logged in the user '" ++ T.unpack name ++ "'."

        return $ if S.member name loggings
          then (u, Nothing)
          else (u { _loggedInUsers = S.insert name loggings }
               , Just $ User ref)

logout :: User -> UserVolatile -> IO ()
logout (User ref) (UserVolatile insides) = modifyMVar insides $
    \u@(UserVolatileInsides loggings) -> do
        name <- readIORef ref
        return (u { _loggedInUsers = S.delete name loggings }, ())

logoutRaw :: T.Text -> UserVolatile -> IO ()
logoutRaw name (UserVolatile insides) = modifyMVar insides $
    \u@(UserVolatileInsides loggings) ->
        return (u { _loggedInUsers = S.delete name loggings }, ())

