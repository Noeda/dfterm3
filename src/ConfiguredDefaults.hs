-- | The purpose of this module is to expose configuration that is set at
-- compile-time.
--

{-# LANGUAGE CPP #-}

module ConfiguredDefaults
    ( defaultStorageDirectory )
    where

import System.Directory

#ifndef DEFAULT_STORAGE_DIRECTORY
#define DEFAULT_STORAGE_DIRECTORY getAppUserDataDirectory "dfterm3"
#endif

-- | Which directory Dfterm3 will try to use by default for storing its data.
defaultStorageDirectory :: IO FilePath
defaultStorageDirectory = DEFAULT_STORAGE_DIRECTORY

