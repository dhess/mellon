module System.Mellon.Lock
         ( Lock(..)
         ) where

import Control.Monad.IO.Class

-- | 'Lock' abstracts the machinery which manipulates the actual lock.
-- The protocol is the simplest one possible: 'lock' and 'unlock'.
class Lock l where
  lock :: MonadIO m => l -> m ()
  unlock :: MonadIO m => l -> m ()
