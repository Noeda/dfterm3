-- | Internal module for `Dfterm3.GameSubscription`; implements acid state
-- transactions.

module Dfterm3.GameSubscription.Internal.Transactions
    (
      tryPublishGame
    , tryRemoveGame
    , getPublishedGames
    )
    where

import Dfterm3.Dfterm3State.Internal.Types
import Dfterm3.GameSubscription.Internal.Types
import Control.Lens
import Data.Acid

import qualified Data.Map.Strict as M
import qualified Data.ByteString as B

publishedGames' :: Lens' PersistentStorageState
                         (M.Map B.ByteString (B.ByteString, B.ByteString))
publishedGames' = gameSubscriptions . publishedGames

tryPublishGame :: B.ByteString
               -> (B.ByteString, B.ByteString)
               -> Update PersistentStorageState Bool
tryPublishGame key game = do
    old_games <- use publishedGames'
    publishedGames' %= M.insert key game
    return $ case M.lookup key old_games of
                 Nothing -> False
                 Just  _ -> True

tryRemoveGame :: B.ByteString
              -> Update PersistentStorageState Bool
tryRemoveGame key = do
    old_games <- use publishedGames'
    publishedGames' %= M.delete key
    return $ case M.lookup key old_games of
                 Nothing -> False
                 Just  _ -> True

getPublishedGames :: Query PersistentStorageState (M.Map
                                                   B.ByteString
                                                   ( B.ByteString
                                                   , B.ByteString ))
getPublishedGames = view publishedGames'

