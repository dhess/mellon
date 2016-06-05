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
       , lockDevice
       , unlockDevice
       , MockLock
       , mockLock
       , MockLockEvent(..)
       , lockMockLock
       , unlockMockLock
       , events
       , mockLockDevice
       ) where

import Control.Concurrent (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics

data Device d =
  Device {_lockDevice :: IO ()
         ,_unlockDevice :: IO ()}

lockDevice :: (MonadIO m) => Device d -> m ()
lockDevice = liftIO . _lockDevice

unlockDevice :: (MonadIO m) => Device d -> m ()
unlockDevice = liftIO . _unlockDevice

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

data MLE = MLL | MLU deriving (Eq)

lockMockLock :: (MonadIO m) => MockLock -> m ()
lockMockLock = updateMockLock MLL

unlockMockLock :: (MonadIO m) => MockLock -> m ()
unlockMockLock = updateMockLock MLU

mockLockDevice :: MockLock -> Device MockLock
mockLockDevice l =
  Device (liftIO $ lockMockLock l)
         (liftIO $ unlockMockLock l)

-- | Helpers
updateMockLock :: (MonadIO m) => MLE -> MockLock -> m ()
updateMockLock mle (MockLock m) = liftIO $
  do now <- getCurrentTime
     ev <- takeMVar m
     putMVar m (mappend ev [event now])
  where
    event :: UTCTime -> MockLockEvent
    event =
      case mle of
        MLL -> LockEvent
        MLU -> UnlockEvent
