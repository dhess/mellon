-- | A dummy 'LockDevice' implementation that simply logs lock/unlock
-- events, including timestamps. Useful for testing and debugging.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mellon.MockLock
       ( MockLock
       , MockLockEvent(..)
       , events
       , mockLock
       ) where

import Control.Concurrent (MVar, newMVar, putMVar, readMVar, takeMVar)
import Data.Time (UTCTime, getCurrentTime)
import System.Mellon.LockDevice

-- | The locking events logged by 'MockLock'.
data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show)

-- | A mock lock device that logs lock/unlock events.
--
-- No constructor is exported. Use 'mockLock' to create a new
-- instance.
data MockLock =
  MockLock (MVar [MockLockEvent])
  deriving (Eq)

-- | Extract the current log of events from the mock lock.
events :: MockLock -> IO [MockLockEvent]
events (MockLock m) = readMVar m

-- | Construct a new mock lock with an empty event log.
mockLock :: IO MockLock
mockLock =
  do m <- newMVar []
     return $ MockLock m

instance LockDevice MockLock where
  lockDevice (MockLock m) =
    do now <- getCurrentTime
       ev <- takeMVar m
       putMVar m (ev ++ [LockEvent now])
  unlockDevice (MockLock m) =
    do now <- getCurrentTime
       ev <- takeMVar m
       putMVar m (ev ++ [UnlockEvent now])
