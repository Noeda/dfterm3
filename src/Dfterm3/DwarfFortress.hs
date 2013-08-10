-- | This module talks to Dfhack to get Dwarf Fortress games into Dfterm3.

{-# LANGUAGE Rank2Types, DeriveDataTypeable, FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell #-}

module Dfterm3.DwarfFortress
    ( monitorDwarfFortresses
    , DwarfFortress(..)
    , DwarfFortressCP437()
    , game, df
    , dfExecutable
    , dfArgs
    , dfWorkingDirectory )
    where

import Dfterm3.GamePool
import Dfterm3.CP437Game
import Dfterm3.Logging
import Dfterm3.Safe

import System.IO
import System.IO.Error
import System.Random

import Data.Word ( Word8, Word16, Word32, Word64 )
import Foreign.Storable ( sizeOf )
import Control.Lens
import Data.Typeable
import Data.Foldable
import Data.ProtocolBuffers
import Data.Array
import Data.TypeLevel hiding ( Eq, (-), (*) )
import Data.Bits
import GHC.Generics ( Generic )
import Control.Concurrent
import Control.Monad ( forever, void, forM, replicateM )
import Control.Exception
import qualified Data.Text as T
import qualified Data.ByteString as B
import Network

import qualified Data.Serialize.Get as S
import qualified Data.Serialize.Put as S

data SilentlyDie = SilentlyDie
                   deriving ( Eq, Show, Typeable )

instance Exception SilentlyDie

data DwarfFortress = DwarfFortress { _dfExecutable :: FilePath
                                   , _dfArgs :: [String]
                                   , _dfWorkingDirectory :: FilePath }
                     deriving ( Eq, Ord, Show, Read, Typeable )
makeLenses ''DwarfFortress

-- | A wrapping around `CP437Game` so that DwarfFortress has unique type in a
-- `GamePool`.
data DwarfFortressCP437 = DwarfFortressCP437 { game :: CP437Game
                                             , df :: DwarfFortress }
                          deriving ( Typeable )

ports :: [Word16]
ports = [48000..48100]

-- | Monitors system ports for Dwarf Fortress processes and registers them to
-- the game pool as they are found.
--
-- This launches a thread. You can safely kill it if you want it gone. This
-- call unmasks asynchronous exceptions for the returned ThreadId.
monitorDwarfFortresses :: GamePool -> IO ThreadId
monitorDwarfFortresses pool =
    forkIOWithUnmask $ \um -> um $ mask $ \unmask -> do

    tids <- forM ports $ \port -> forkIOWithUnmask $ portChecker pool port
    finally (unmask $ forever $ threadDelay 1000000000) $
        forM_ tids $ \tid -> killThread tid

portChecker :: GamePool -> Word16 -> (forall a. IO a -> IO a) -> IO ()
portChecker pool port unmask = forever $ do
    is_done <- newEmptyMVar
    tid <- forkIO $ finally (unmask $ portChecker' pool port) $
                        putMVar is_done ()
    finally (unmask $ takeMVar is_done) $
        killThread tid
    -- Have somewhat random delay.
    -- This is to evenly distribute load.
    delay <- randomRIO (5000000, 20000000)
    threadDelay delay

portChecker' :: GamePool -> Word16 -> IO ()
portChecker' pool port = catch (do
    -- squelch error messages in case of an IO error. That's why we use
    -- tryIOError instead of just letting it fail.
    maybe_handle <- tryIOError $
                        connectTo "127.0.0.1" (PortNumber $ fromIntegral port)
    case maybe_handle of
        Right handle -> finally (dfhackConnection pool handle) (hClose handle)
        Left _ -> return ())
        (\e -> do
            e `seq` void $ return (e :: SilentlyDie)
            return ())

--silentlyDie :: IO a
--silentlyDie = throwIO SilentlyDie

-- The protocol buffer thingy. Keep in sync with dfhack:
-- plugins/proto/dfterm3.proto in the dfhack repository.

data Introduction = Introduction
    { dfVersion        :: Optional D1 (Value T.Text)
    , workingDirectory :: Optional D2 (Value T.Text)
    , executable       :: Optional D3 (Value T.Text)
    , pid              :: Optional D4 (Value Word64) }
    deriving ( Generic, Show )

instance Decode Introduction

data ScreenData = ScreenData
    { width :: Optional D5 (Value Word32)
    , height :: Optional D6 (Value Word32)
    , screenCP437 :: Optional D7 (Value B.ByteString)
    , colors :: Optional D8 (Value B.ByteString) }
    deriving ( Generic, Show )

instance Decode ScreenData

hGetOrDie :: Handle -> Int -> IO B.ByteString
hGetOrDie _ 0          = return B.empty
hGetOrDie handle bytes = do
    result <- B.hGet handle bytes
    if B.null result
      then error "Connection lost."
      else return result

hGetMessage :: Decode a => Handle -> IO a
hGetMessage handle = do
    len_bs <- hGetOrDie handle (sizeOf (undefined :: Word32))
    let Right len = S.runGet S.getWord32be len_bs
    msg <- hGetOrDie handle (fromIntegral len)
    case S.runGet decodeMessage msg of
        Left _ -> error "Malformed data received from Dfhack to Dfterm3."
        Right x -> return x

hSendByteString :: Handle -> B.ByteString -> IO ()
hSendByteString handle bytestring = do
    let len_bs = S.runPut $
                     S.putWord32be (safeFromIntegral $ B.length bytestring)
    B.hPut handle len_bs
    B.hPut handle bytestring
    hFlush handle

instance Game DwarfFortressCP437 () CP437Changes

dfhackConnection :: GamePool -> Handle -> IO ()
dfhackConnection pool handle = do
    info <- hGetMessage handle :: IO Introduction
    let Just version = getField $ dfVersion info
        Just working_dir = getField $ workingDirectory info
        Just df_executable = getField $ executable info
        df_info = DwarfFortress (T.unpack df_executable)
                                []
                                (T.unpack working_dir)
    -- Not interested in pid for the moment
    -- maybe_pid <- getField $ pid msg

    cookie_contents <- readMagicCookieFile (T.unpack working_dir ++
                                            "/.dfterm3-cookie")
    hSendByteString handle cookie_contents

    msg <- hGetMessage handle :: IO ScreenData

    logInfo "Successfully formed a connection to a Dfhack \
            \Dfterm3 plugin."
    ( provider , _) <- registerGame pool

    let title = "Dwarf Fortress " `T.append` version

    rec title (provider :: GameProvider DwarfFortressCP437 () CP437Changes)
              emptyCP437Changes
              df_info
              msg
  where
    rec title provider changer df_info msg = do
        let Just cp437Data = getField $ screenCP437 msg
            Just colorsData = getField $ colors msg
            Just w = getField $ width msg
            Just h = getField $ height msg

            assocs = [(fromIntegral x, fromIntegral y) |
                      y <- [0..h-1], x <- [0..w-1]]
            assocs_len = fromIntegral $ w*h
            Right chars =
                flip S.runGet cp437Data $ replicateM assocs_len S.getWord8
            Right clrs =
                flip S.runGet colorsData $ replicateM assocs_len S.getWord8


            cp437game = newCP437Game title $
                            array ( (0, 0)
                                  , (fromIntegral w-1, fromIntegral h-1) )
                                  (zip assocs $ zipWith cp437nator chars clrs)

        updateGame (DwarfFortressCP437 cp437game df_info)
                   (changer cp437game)
                   provider

        rec title provider (findCP437Changes cp437game) df_info
            =<< hGetMessage handle

cp437nator :: Word8 -> Word8 -> CP437Cell
cp437nator character_code colors =
    CP437Cell character_code foreground_color background_color
  where
    foreground_color = intToColor $ fromIntegral $
        (colors .&. 0xf0) `shift` (-4)
    background_color = intToColor $ fromIntegral (colors .&. 0x0f)
{-# INLINE cp437nator #-}


readMagicCookieFile :: FilePath -> IO B.ByteString
readMagicCookieFile = B.readFile

