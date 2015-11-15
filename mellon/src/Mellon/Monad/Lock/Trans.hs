-- | A monadic interface for locking and unlocking 'Device's.

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Mellon.Monad.Lock.Trans
       ( -- * The Lock monad.
         Lock
       , LockT(..)
       , runLock
       , runLockT
       , lockDevice
       , unlockDevice
       ) where

import Control.Applicative (Alternative)
import Control.Monad.Reader
import Mellon.Device.Class (Device)
import qualified Mellon.Device.Class as D (lockDevice, unlockDevice)

-- | A monad transformer which adds a 'Device' to an existing
-- monad.
newtype LockT d m a =
  LockT { unLockT :: ReaderT d m a }
  deriving (Alternative,Applicative,Functor,Monad,MonadReader d,MonadTrans,MonadIO,MonadFix,MonadPlus)

-- | Run an action inside the 'LockT' transformer using the specified 'Device'
-- and return the result.
runLockT :: (Monad m, Device d) => d -> LockT d m a -> m a
runLockT d action = runReaderT (unLockT action) d

-- | The simplest useful 'LockT' monad.
type Lock d a = LockT d IO a

-- | Run an action inside the 'Lock' monad using the supplied
-- 'Device'.
runLock :: (Device d) => d -> Lock d a -> IO a
runLock = runLockT

-- | Lock the device.
lockDevice :: (MonadIO m, Device d) => LockT d m ()
lockDevice = device >>= liftIO . D.lockDevice

-- | Unlock the device.
unlockDevice :: (MonadIO m, Device d) => LockT d m ()
unlockDevice = device >>= liftIO . D.unlockDevice


-- Internal actions.
--
device :: (Monad m) => LockT d m d
device = ask
