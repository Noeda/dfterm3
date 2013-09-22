-- | Users.
--
-- Users are players on the server. They may chat, they may play. They are
-- uniquely identified by their name.
--

{-# LANGUAGE DeriveDataTypeable #-}

module Dfterm3.User
    ( -- * Creating users.
      newGuestUser
    , loginUser
    , whenLoggedIn
    , User()
    )
    where

import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.Util

import Data.Typeable ( Typeable )
import Data.Foldable ( forM_ )
import Data.IORef
import Control.Monad.IO.Class ( MonadIO, liftIO )
import Control.Exception ( mask_ )
import Control.Lens

import qualified Data.Set as S
import qualified Data.Text as T

data UserInsides = Guest
                 | LoggedInUser T.Text
                 deriving ( Eq, Ord, Show, Read, Typeable )

-- | Handle to a user.
newtype User = User (IORef (IORef UserInsides))
               deriving ( Eq, Typeable )

-- | Create a guest user.
newGuestUser :: Storage -> IO User
newGuestUser ps = do
    ref <- newIORef Guest
    ref2 <- newFinalizableIORef ref $ do
        r <- readIORef ref
        case r of
            Guest -> return ()
            LoggedInUser name -> modifyVolatileStorage' ps $
                                 over loggedInUsers $
                                 S.delete name

    return (User ref2)

-- | Make user log in as another user.
--
-- There cannot be two separate non-guest `User` values that use the same name.
-- If logging in fails, then `False` is returned.
loginUser :: User -> T.Text -> Storage -> IO Bool
loginUser (User ref) name ps
    | T.null name = return False
    | T.length name > 30 = return False
    | otherwise = mask_ $ do
    inside_ref <- readIORef ref
    old_name <- readIORef inside_ref

    result <- modifyVolatileStorage ps $ \old ->
        if S.member name (_loggedInUsers old)
          then ( old, False )
          else ( over loggedInUsers
                      (case old_name of
                           Guest ->
                               S.insert name
                           LoggedInUser oname ->
                               S.insert name . S.delete oname) old
               , True )

    if result
      then do inside_ref <- readIORef ref
              writeIORef inside_ref (LoggedInUser name)
              return True
      else return False

-- | Execute an action only if user has been logged in.
--
-- This extracts the name from the user and that name is passed to the given
-- action.
whenLoggedIn :: (Monad m, MonadIO m) => User -> (T.Text -> m ()) -> m ()
whenLoggedIn (User ref) action = do
    is_logged_in <- liftIO $ do
        ref_insides <- readIORef ref
        insides <- readIORef ref_insides
        return $ case insides of
                     Guest -> Nothing
                     LoggedInUser name -> Just name

    forM_ is_logged_in action

