-- | A lock monad.
--
-- The interface is defined by the 'Mellon.Lock.Monad.Class.MonadLock'
-- typeclass. Note that the typeclass interface is not dependent on
-- the 'LockDevice' interface; 'Lock' and 'LockT' assume you will be
-- wrapping a 'LockDevice' in a 'MonadLock' monad, but other
-- implementations of 'MonadLock' are possible.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.Lock.Monad
       ( -- * Classes
         module Mellon.Lock.Monad.Class
         -- * Monads and monad transformers
       , LockT
       , liftLockT
       , runLockT
       , Lock
       , liftLock
       , runLock
       ) where

import Control.Applicative (Alternative)
import Control.Monad.Identity
import Control.Monad.Reader
import Mellon.Lock.Monad.Class
import Mellon.Lock.Device

-- | A monad transformer which adds a lock device to an existing
-- monad.
newtype LockT l m a =
  LockT (ReaderT l m a)
  deriving (Alternative,Applicative,Functor,Monad,MonadTrans,MonadIO,MonadFix,MonadPlus)

instance (MonadIO m, LockDevice l) => MonadLock (LockT l m) where
  lock = LockT ask >>= liftIO . lockDevice
  unlock = LockT ask >>= liftIO . unlockDevice

-- | Run an action inside the LockT transformer using lock device 'l'
-- and return the result.
runLockT :: (Monad m, LockDevice l) => l -> LockT l m a -> m a
runLockT device (LockT action) = runReaderT action device

-- | Lift an action into LockT.
liftLockT :: (Monad m, LockDevice l) => (l -> m a) -> LockT l m a
liftLockT = LockT . ReaderT

-- | A basic lock device monad.
type Lock l = LockT l Identity

-- | Lift an action into Lock.
liftLock :: (LockDevice l) => (l -> a) -> Lock l a
liftLock = LockT . reader

-- | Run a lock computation using the lock device 'l' and return the
-- result.
runLock :: (LockDevice l) => l -> Lock l a -> a
runLock l x = runIdentity (runLockT l x)
