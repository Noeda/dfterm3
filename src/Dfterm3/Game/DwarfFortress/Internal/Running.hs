-- | Internal module to `Dfterm3.Game.DwarfFortress`.
--

module Dfterm3.Game.DwarfFortress.Internal.Running
    ( DFPid
    , DFState()
    , newDFState
    , allAliveInstances
    , ProcureStatus(..)
    , takeOldOrStartProcuring
    , registerNew
    , unregister )
    where

import Data.Word
import Data.IORef
import Data.Maybe ( catMaybes )
import Control.Monad
import Control.Exception ( mask_ )
import Control.Concurrent.MVar
import qualified Data.ByteString as B
import qualified Data.Map as M

type DFPid = Word64
newtype DFState a = DFState (IORef (M.Map B.ByteString (DFStatus a)))

newDFState :: IO (DFState a)
newDFState = DFState `fmap` newIORef M.empty

data DFStatus a = Procuring (MVar a)
                | Alive a

data ProcureStatus a = PleaseProcure (MVar a)
                     | AlreadyProcuring (MVar a)

allAliveInstances :: DFState a -> IO [a]
allAliveInstances (DFState vals) = do
    insides <- readIORef vals
    return $ catMaybes $ fmap mapping (M.elems insides)
  where
    mapping (Alive x) = Just x
    mapping _ = Nothing

takeOldOrStartProcuring :: B.ByteString -> DFState a -> IO (ProcureStatus a)
takeOldOrStartProcuring key (DFState vals) = mask_ $ do
    mvar <- newEmptyMVar
    join $ atomicModifyIORef' vals $ \old ->
        case M.lookup key old of
            Nothing ->
                ( M.insert key (Procuring mvar) old
                , return $ PleaseProcure mvar )
            Just ( Alive x ) ->
                ( old, putMVar mvar x >> return (AlreadyProcuring mvar) )
            Just ( Procuring mvar' ) ->
                ( old, return $ AlreadyProcuring mvar' )

registerNew :: B.ByteString -> DFState a -> a -> IO ()
registerNew key (DFState vals) val = mask_ $
    join $ atomicModifyIORef' vals $ \old ->
        case M.lookup key old of
            Nothing ->
                ( M.insert key (Alive val) old, return () )
            Just ( Alive _ ) ->
                error $ "Same Dwarf Fortress ('" ++ show key ++ "') twice."
            Just ( Procuring mvar ) ->
                ( M.insert key (Alive val) old
                , putMVar mvar val )

unregister :: B.ByteString -> DFState a -> IO ()
unregister key (DFState vals) = mask_ $
    atomicModifyIORef' vals $ \old ->
        ( M.delete key old, () )

