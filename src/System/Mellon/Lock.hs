-- | 'MonadLock' is a very simple interface for implementing lock devices
-- in @mellon@. The core @mellon@ library does not include any
-- physical 'MonadLock' implementations, but it does include a "mock
-- lock" implementation for debugging and testing.

{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mellon.Lock
       ( -- * Classes
         module System.Mellon.Lock.Class
         -- * A mock lock type for debugging.
       , MockLock
       , MockLockEvent(..)
       , MockLockT
       , events
       , mockLock
       , runMockLockT
       ) where

import Control.Applicative (Alternative)
import Control.Concurrent (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Monad.Reader
import Data.Time (UTCTime, getCurrentTime)
import System.Mellon.Lock.Class

-- | The locking events logged by 'MockLockT'.
data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show)

data MockLock =
  MockLock (MVar [MockLockEvent])
  deriving (Eq)

events :: MockLock -> IO [MockLockEvent]
events (MockLock m) =
  do ev <- liftIO $ takeMVar m
     liftIO $ putMVar m ev
     return ev

mockLock :: IO MockLock
mockLock =
  do m <- newEmptyMVar
     putMVar m []
     return $ MockLock m

newtype MockLockT m a =
  MockLockT (ReaderT MockLock m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

-- | Note that the monad wrapped by 'MockLockT' must be an instance of
-- 'MonadIO', as 'MockLockT' uses 'getCurrentTime' to get the locking
-- event timestamps.
instance (MonadIO m) => MonadLock (MockLockT m) where
  lock =
    do now <- liftIO $ getCurrentTime
       (MockLock m) <- MockLockT ask
       ev <- liftIO $ takeMVar m
       liftIO $ putMVar m (ev ++ [LockEvent now])
  unlock =
    do now <- liftIO $ getCurrentTime
       (MockLock m) <- MockLockT ask
       ev <- liftIO $ takeMVar m
       liftIO $ putMVar m (ev ++ [UnlockEvent now])

runMockLockT :: (MonadIO m) => MockLock -> MockLockT m a -> m a
runMockLockT m (MockLockT action) = runReaderT action m
