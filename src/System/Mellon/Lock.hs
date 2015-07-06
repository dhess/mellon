-- | 'Lock' abstracts the machinery which manipulates an actual
-- lock device.

module System.Mellon.Lock
         ( Lock(..)
         ) where

import Control.Monad.IO.Class

-- | A simple protocol for lock devices.
class Lock l where
  lock :: MonadIO m => l -> m ()
  unlock :: MonadIO m => l -> m ()

  -- | Perform cleanup and release any exclusive state.
  --
  -- After invoking this function, the 'Lock' is no longer valid.
  quit :: MonadIO m => l -> m ()
