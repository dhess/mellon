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
import Data.Functor.Identity (Identity, runIdentity)
import System.Mellon.Lock.Class

newtype MockLockT m a =
  MockLockT (WriterT [String] m a)
  deriving (Applicative,Foldable,Functor,Monad,MonadTrans,MonadIO,MonadFix,Traversable,Alternative,MonadPlus,Show,Read,Ord,Eq)

liftMockLockT :: (Monad m) => m (a, [String]) -> MockLockT m a
liftMockLockT = MockLockT . WriterT

runMockLockT :: (Monad m) => MockLockT m a -> m (a, [String])
runMockLockT (MockLockT x) = runWriterT x

evalMockLockT :: (Monad m) => MockLockT m a -> m a
evalMockLockT (MockLockT x) = liftM fst (runWriterT x)

execMockLockT :: (Monad m) => MockLockT m a -> m [String]
execMockLockT (MockLockT x) = execWriterT x

type MockLock a = MockLockT Identity a

liftMockLock :: (a, [String]) -> MockLock a
liftMockLock = MockLockT . writer

runMockLock :: MockLock a -> (a, [String])
runMockLock = runIdentity . runMockLockT

evalMockLock :: MockLock a -> a
evalMockLock = runIdentity . evalMockLockT

execMockLock :: MockLock a -> [String]
execMockLock = runIdentity . execMockLockT

instance (Monad m) => MonadLock (MockLockT m) where
  lock = MockLockT $ tell ["Locked"]
  unlock = MockLockT $ tell ["Unlocked"]
