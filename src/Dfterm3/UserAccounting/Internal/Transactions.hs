module Dfterm3.UserAccounting.Internal.Transactions
    ( areGuestsEnabled
    , areRegistrationsEnabled
    , isValidLogin )
    where

import Dfterm3.UserAccounting.Internal.Types
import Dfterm3.Dfterm3State.Internal.Types
import Data.Acid
import Control.Lens
import Crypto.Scrypt
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map.Strict as M
import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe

areGuestsEnabled :: Query PersistentStorageState Bool
areGuestsEnabled = _allowGuests . _userAccounting <$> ask

areRegistrationsEnabled :: Query PersistentStorageState Bool
areRegistrationsEnabled = _allowRegistrations . _userAccounting <$> ask

isValidLogin :: T.Text -> T.Text -> Update PersistentStorageState Bool
isValidLogin username pw = do
    st <- _userAccounting <$> get
    case M.lookup username (_userAccounts st) of
        Nothing -> return False
        Just account ->
            if isNothing (_password account)
              then return False
              else case verifyPass
                            defaultParams
                            (Pass $ T.encodeUtf8 pw)
                            (EncryptedPass (fromJust (_password account))) of
                       (False, _) -> return False
                       (True, Nothing) -> return True
                       (True, Just (EncryptedPass new_pass)) -> do
                           (userAccounting . userAccounts) %=
                               M.adjust (password .~ Just new_pass)
                                        username
                           return True

