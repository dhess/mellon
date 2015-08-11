{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module System.Mellon.Lock
       ( module System.Mellon.Lock.Class
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

data Event
  = Locked UTCTime
  | Unlocked UTCTime
  deriving (Eq,Show)

newtype MockLockT m a =
  MockLockT (WriterT [Event] m a)
  deriving (Applicative,Foldable,Functor,Monad,MonadTrans,MonadIO,MonadFix,Traversable,Alternative,MonadPlus,Show,Eq)

liftMockLockT :: (MonadIO m) => m (a, [Event]) -> MockLockT m a
liftMockLockT = MockLockT . WriterT

runMockLockT :: (MonadIO m) => MockLockT m a -> m (a, [Event])
runMockLockT (MockLockT x) = runWriterT x

evalMockLockT :: (MonadIO m) => MockLockT m a -> m a
evalMockLockT (MockLockT x) = liftM fst (runWriterT x)

execMockLockT :: (MonadIO m) => MockLockT m a -> m [Event]
execMockLockT (MockLockT x) = execWriterT x

type MockLock a = MockLockT IO a

liftMockLock :: (a, [Event]) -> MockLock a
liftMockLock = MockLockT . writer

runMockLock :: MockLock a -> IO (a, [Event])
runMockLock = runMockLockT

evalMockLock :: MockLock a -> IO a
evalMockLock = evalMockLockT

execMockLock :: MockLock a -> IO [Event]
execMockLock = execMockLockT

instance (MonadIO m) => MonadLock (MockLockT m) where
  lock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [Locked now]
  unlock =
    do now <- liftIO $ getCurrentTime
       MockLockT $ tell [Unlocked now]
