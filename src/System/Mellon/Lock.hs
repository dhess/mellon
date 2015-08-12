{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mellon.Lock
       ( module System.Mellon.Lock.Class
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

data MockLockEvent
  = LockEvent !UTCTime
  | UnlockEvent !UTCTime
  deriving (Eq,Show)

newtype MockLockT m a =
  MockLockT (WriterT [MockLockEvent] m a)
  deriving (Applicative,Foldable,Functor,Monad,MonadTrans,MonadIO,MonadFix,Traversable,Alternative,MonadPlus,Show,Eq)

liftMockLockT :: (MonadIO m) => m (a, [MockLockEvent]) -> MockLockT m a
liftMockLockT = MockLockT . WriterT

runMockLockT :: (MonadIO m) => MockLockT m a -> m (a, [MockLockEvent])
runMockLockT (MockLockT x) = runWriterT x

evalMockLockT :: (MonadIO m) => MockLockT m a -> m a
evalMockLockT (MockLockT x) = liftM fst (runWriterT x)

execMockLockT :: (MonadIO m) => MockLockT m a -> m [MockLockEvent]
execMockLockT (MockLockT x) = execWriterT x

type MockLock a = MockLockT IO a

liftMockLock :: (a, [MockLockEvent]) -> MockLock a
liftMockLock = MockLockT . writer

runMockLock :: MockLock a -> IO (a, [MockLockEvent])
runMockLock = runMockLockT

evalMockLock :: MockLock a -> IO a
evalMockLock = evalMockLockT

execMockLock :: MockLock a -> IO [MockLockEvent]
execMockLock = execMockLockT

instance (MonadIO m) => MonadLock (MockLockT m) where
  lock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [LockEvent now]
  unlock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [UnlockEvent now]
