{-|
Module      : Mellon.Device
Description : An interface for physical access devices
Copyright   : (c) 2018, Quixoftic, LLC
License     : BSD3
Maintainer  : Drew Hess <dhess-src@quixoftic.com>
Stability   : experimental
Portability : non-portable

This module provides both a parameterized type for adapting a
device-specific interface to the generic interface expected by a
'Mellon.Controller.Controller', and a "mock lock" device
implementation, which is useful for testing.

-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe #-}

module Mellon.Device
       ( -- * The mellon-core device type
         Device(..)

         -- * A mock lock implementation
         --
         -- | The mock lock type provided here logs lock / unlock
         -- events along with a timestamp. It is useful for testing
         -- but doesn't have any facility to control an actual
         -- physical access device.
       , MockLock
       , mockLock
       , MockLockEvent(..)
       , lockMockLock
       , unlockMockLock
       , events
       , mockLockDevice
       ) where

import Control.Concurrent
       (MVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Data
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics

-- | A parametric device type which provides two "methods," one to
-- lock the device, and the other to unlock it.
--
-- The parameter @d@ is the concrete device type and is used during
-- construction to create the two methods by binding them to actions
-- on the specific device.
--
-- For example, the implementation of the 'mockLockDevice' function,
-- which wraps a 'MockLock' in a 'Device' @d@, looks like this:
--
-- > mockLockDevice :: MockLock -> Device MockLock
-- > mockLockDevice l =
-- >   Device (liftIO $ lockMockLock l)
-- >          (liftIO $ unlockMockLock l)
--
-- A program can construct such a device and use it like so:
--
-- >>> ml <- mockLock
-- >>> let mld = mockLockDevice ml
-- >>> events ml
-- []
-- >>> lockDevice mld
-- >>> events ml
-- [LockEvent ... UTC]
-- >>> unlockDevice mld
-- >>> events ml
-- [LockEvent ... UTC,UnlockEvent ... UTC]
data Device d =
  Device {lockDevice :: IO ()
         ,unlockDevice :: IO ()}

-- | Events logged by 'MockLock' are of this type.
data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show,Read,Generic,Data,Typeable)

-- | A mock lock device that logs lock / unlock events.
--
-- No constructor is exported. Use 'mockLock' to create a new
-- instance and 'events' to extract the log.
data MockLock =
  MockLock !(MVar [MockLockEvent])
  deriving (Eq)

-- | Construct a new mock lock with an empty event log.
mockLock :: (MonadIO m) => m MockLock
mockLock = liftIO $ MockLock <$> newMVar []

-- | Extract the current log of events from the mock lock.
events :: (MonadIO m) => MockLock -> m [MockLockEvent]
events (MockLock m) = liftIO $ readMVar m

data MLE = MLL | MLU deriving (Eq)

-- | Lock the mock lock.
lockMockLock :: (MonadIO m) => MockLock -> m ()
lockMockLock = updateMockLock MLL

-- | Unlock the mock lock.
unlockMockLock :: (MonadIO m) => MockLock -> m ()
unlockMockLock = updateMockLock MLU

-- | Wrap a 'MockLock' value with a 'Device' value, for use with a
-- @mellon-core@ controller.
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
