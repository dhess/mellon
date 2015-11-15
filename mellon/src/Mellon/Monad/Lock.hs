-- | The 'Lock' monad.

module Mellon.Monad.Lock
  ( -- * 'MonadLock' class
    module Mellon.Monad.Lock.Class
    -- * 'LockT' monad transformer
  , Lock
  , LockT
  , runLock
  , runLockT
  ) where

import Mellon.Monad.Lock.Class
import Mellon.Monad.Lock.Trans (Lock, LockT, runLock, runLockT)
