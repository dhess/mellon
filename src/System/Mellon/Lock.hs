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
       , MockLockEvent(..)
       , MockLockT
       , MockLock
       , evalMockLockT
       , evalMockLock
       , execMockLock
       , execMockLockT
       , liftMockLock
       , liftMockLockT
       , runMockLock
       , runMockLockT
       ) where

import Control.Applicative (Alternative)
import Control.Monad.Writer
import Data.Time (UTCTime, getCurrentTime)
import System.Mellon.Lock.Class

-- | The locking events logged by 'MockLockT'.
data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show)

-- | 'MockLockT' is a monad transformer that adds 'MonadLock'
-- functionality to 'WriterT'. Each time a 'System.Mellon.Controller'
-- locks or unlocks a 'MockLockT' lock, the 'MockLockT' will log the
-- event, along with its timestamp, to the 'WriterT'.
newtype MockLockT m a =
  MockLockT (WriterT [MockLockEvent] m a)
  deriving (Applicative,Foldable,Functor,Monad,MonadTrans,MonadIO,MonadFix,Traversable,Alternative,MonadPlus,Show,Eq)

-- | Note that the monad wrapped by 'MockLockT' must be an instance of
-- 'MonadIO', as 'MockLockT' uses 'getCurrentTime' to get the locking
-- event timestamps.
instance (MonadIO m) => MonadLock (MockLockT m) where
  lock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [LockEvent now]
  unlock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [UnlockEvent now]

-- | Lift an action into the wrapped 'WriterT' transformer.
liftMockLockT :: (MonadIO m) => m (a, [MockLockEvent]) -> MockLockT m a
liftMockLockT = MockLockT . WriterT

-- | Run the 'MockLockT' and return both the value of the computation, and the list of
-- logged 'MockLockEvent's.
runMockLockT :: (MonadIO m) => MockLockT m a -> m (a, [MockLockEvent])
runMockLockT (MockLockT x) = runWriterT x

-- | Run the 'MockLockT' and return just the value of the computation.
evalMockLockT :: (MonadIO m) => MockLockT m a -> m a
evalMockLockT (MockLockT x) = liftM fst (runWriterT x)

-- | Run the 'MockLockT' and return just the list of logged
-- 'MockLockEvent's, discarding the value of the computation.
execMockLockT :: (MonadIO m) => MockLockT m a -> m [MockLockEvent]
execMockLockT (MockLockT x) = execWriterT x

-- | 'MockLock' wraps the 'IO' monad in a 'MockLockT'.
type MockLock a = MockLockT IO a

-- | Lift an action into the wrapped 'WriterT' transformer.
liftMockLock :: (a, [MockLockEvent]) -> MockLock a
liftMockLock = MockLockT . writer

-- | Run the 'MockLock' and return both the value of the computation, and the list of
-- logged 'MockLockEvent's.
runMockLock :: MockLock a -> IO (a, [MockLockEvent])
runMockLock = runMockLockT

-- | Run the 'MockLock' and return just the value of the computation.
evalMockLock :: MockLock a -> IO a
evalMockLock = evalMockLockT

-- | Run the 'MockLock' and return just the list of logged
-- 'MockLockEvent's, discarding the value of the computation.
execMockLock :: MockLock a -> IO [MockLockEvent]
execMockLock = execMockLockT

