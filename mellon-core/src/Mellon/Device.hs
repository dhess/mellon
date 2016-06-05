{-|
Module      : Mellon.Device
Description : Actions on physical access devices
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module Mellon.Device
       ( Device(..)
       , MockLock
       , mockLock
       , lockMockLock
       , unlockMockLock
       , events
       , mockLockDevice
       , MockLockEvent(..)
       ) where

import Control.Concurrent (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics

data Device d =
  Device {_lockDevice :: IO ()
         ,_unlockDevice :: IO ()}

-- | The locking events logged by 'MockLock'.
data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | A mock lock device that logs lock/unlock events.
--
-- No constructor is exported. Use 'mockLock' to create a new
-- instance.
--
-- XXX TODO: parameterize this on Monoid?
data MockLock =
  MockLock (MVar [MockLockEvent])
  deriving (Eq)

-- | Construct a new mock lock with an empty event log.
mockLock :: (MonadIO m) => m MockLock
mockLock = liftIO $ MockLock <$> newMVar []

-- | Extract the current log of events from the mock lock.
events :: (MonadIO m) => MockLock -> m [MockLockEvent]
events (MockLock m) = liftIO $ readMVar m

lockMockLock :: (MonadIO m) => MockLock -> m ()
lockMockLock (MockLock m) = liftIO $
  do now <- getCurrentTime
     ev <- takeMVar m
     putMVar m (ev ++ [LockEvent now])

unlockMockLock :: (MonadIO m) => MockLock -> m ()
unlockMockLock (MockLock m) = liftIO $
  do now <- getCurrentTime
     ev <- takeMVar m
     putMVar m (ev ++ [UnlockEvent now])

mockLockDevice :: MockLock -> Device MockLock
mockLockDevice l =
  Device (liftIO $ lockMockLock l)
         (liftIO $ unlockMockLock l)
