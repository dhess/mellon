-- | A typeclass for monadic devices.

module Mellon.Monad.Lock.Class
       ( MonadLock(..)
       ) where

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.List
import Control.Monad.Reader
import qualified Control.Monad.RWS.Lazy as RWSL
import qualified Control.Monad.RWS.Strict as RWSS
import qualified Control.Monad.State.Lazy as SL
import qualified Control.Monad.State.Strict as SS
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Writer.Lazy as WL
import qualified Control.Monad.Writer.Strict as WS
import Mellon.Device.Class (Device)
import Mellon.Monad.Lock.Trans (LockT(..))
import qualified Mellon.Monad.Lock.Trans as Lock (lockDevice, unlockDevice)

-- | A device monad interface.
class (MonadIO m) => MonadLock m where
  -- | Lock the device.
  lockDevice :: m ()
  unlockDevice :: m ()

instance (MonadIO m, Device d) => MonadLock (LockT d m) where
  lockDevice = Lock.lockDevice
  unlockDevice = Lock.unlockDevice

instance (MonadLock m) => MonadLock (IdentityT m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (SL.StateT s m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (SS.StateT s m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m, Monoid w) => MonadLock (WL.WriterT w m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m, Monoid w) => MonadLock (WS.WriterT w m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (ReaderT r m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m, Monoid w) => MonadLock (RWSL.RWST r w s m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m, Monoid w) => MonadLock (RWSS.RWST r w s m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (ExceptT e m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (MaybeT m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (ContT r m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice

instance (MonadLock m) => MonadLock (ListT m) where
  lockDevice = lift lockDevice
  unlockDevice = lift unlockDevice
