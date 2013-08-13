-- | The "master" monad that combines the user system, gamepool and volatile
-- user data in a single monad.
--
-- Many places use these handles so it is convenient if they are available
-- whenever needed, without having to explictly passing them around.
--

{-# LANGUAGE Rank2Types, GeneralizedNewtypeDeriving #-}

module Dfterm3.Dfterm3Monad
    ( Dfterm3T()
    , Dfterm3ClientT()
    , Dfterm3IO
    , runDfterm3T
    , runDfterm3ClientT
    , loginM
    , whenLoggedIn
    , withGamePool
    , withUserSystem
    , withUserVolatile )
    where

import Dfterm3.GamePool
import Dfterm3.User
import Dfterm3.UserVolatile
import Dfterm3.Util ( whenJust )

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Reader
import Control.Monad.State.Strict

import qualified Data.Text as T

data Dfterm3Things = Dfterm3Things GamePool
                                   UserSystem
                                   UserVolatile

data Dfterm3ClientThings = Dfterm3ClientThings (Maybe User)

newtype Dfterm3T m a = Dfterm3T
    { runDfterm3T' :: ReaderT Dfterm3Things m a }
    deriving ( Monad, Applicative, Functor, MonadTrans )

-- | Same as `Dfterm3T` but it includes the concept of having logged in.
newtype Dfterm3ClientT m a = Dfterm3ClientT
    (StateT Dfterm3ClientThings (Dfterm3T m) a)
    deriving ( Monad, Applicative, Functor )

runDfterm3T :: Dfterm3T m a -> GamePool -> UserSystem -> UserVolatile -> m a
runDfterm3T action pool us uv = runReaderT (runDfterm3T' action)
                                           (Dfterm3Things pool us uv)

runDfterm3ClientT :: Monad m
                  => Dfterm3ClientT m a
                  -> Maybe User
                  -> GamePool
                  -> UserSystem
                  -> UserVolatile
                  -> m a
runDfterm3ClientT (Dfterm3ClientT action) vol =
    runDfterm3T (evalStateT action (Dfterm3ClientThings vol))

type Dfterm3IO a = Dfterm3T IO a

instance MonadIO m => MonadIO (Dfterm3T m) where
    liftIO = Dfterm3T . liftIO

instance MonadIO m => MonadIO (Dfterm3ClientT m) where
    liftIO = Dfterm3ClientT . liftIO

instance MonadTrans Dfterm3ClientT where
    lift = Dfterm3ClientT . lift . Dfterm3T . lift

class Monad t => Dfterm3Monad t where
    withGamePool :: (GamePool -> t a) -> t a
    withUserSystem :: (UserSystem -> t a) -> t a
    withUserVolatile :: (UserVolatile -> t a) -> t a

liftDfterm3 :: Monad m => Dfterm3T m a -> Dfterm3ClientT m a
liftDfterm3 = Dfterm3ClientT . lift

instance Monad m => Dfterm3Monad (Dfterm3T m) where
    -- Convenience functions to extract the stuff from the above monad
    withGamePool action = do
        Dfterm3Things pool _ _ <- Dfterm3T ask
        action pool

    withUserSystem action = do
        Dfterm3Things _ us _ <- Dfterm3T ask
        action us

    withUserVolatile action = do
        Dfterm3Things _ _ uv <- Dfterm3T ask
        action uv

instance Monad m => Dfterm3Monad (Dfterm3ClientT m) where
    withGamePool action = do
        Dfterm3Things pool _ _ <- liftDfterm3 $ Dfterm3T ask
        action pool

    withUserSystem action = do
        Dfterm3Things _ us _ <- liftDfterm3 $ Dfterm3T ask
        action us

    withUserVolatile action = do
        Dfterm3Things _ _ uv <- liftDfterm3 $ Dfterm3T ask
        action uv

whenLoggedIn :: Monad m => (User -> Dfterm3ClientT m ()) -> Dfterm3ClientT m ()
whenLoggedIn action = Dfterm3ClientT $ do
    Dfterm3ClientThings maybe_user <- get
    whenJust maybe_user (fmap unwrap action)
  where
    unwrap (Dfterm3ClientT x) = x

-- | A convenience over `Dfterm3.UserVolatile.login`, this logs in in the
-- current `Dfterm3ClientT` monad. Returns the the user if succeeds.
--
-- If already logged, the old logging is forgotten, unless this new logging in
-- fails; in that case, the old logging is preserved.
loginM :: (MonadIO m, Monad m) => T.Text -> Dfterm3ClientT m (Maybe User)
loginM name = withUserVolatile $ \uv -> do
    maybe_user <- liftIO $ login name uv
    case maybe_user of
        Nothing -> return Nothing
        Just user -> do
            Dfterm3ClientT $ put $ Dfterm3ClientThings (Just user)
            return $ Just user

