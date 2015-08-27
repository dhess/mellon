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

data MockLock =
  MockLock (MVar [MockLockEvent])
  deriving (Eq)

events :: MockLock -> IO [MockLockEvent]
events (MockLock m) = readMVar m

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
