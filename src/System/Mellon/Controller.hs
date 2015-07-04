module System.Mellon.Controller
       ( Controller(..)
       ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime)

-- | 'Controller' typeclass. 'Controller's combine a 'Lock' with some
-- kind of scheduling mechanism to allow the user to control a lock
-- device.
--
-- Typically, these functions are synchronous; i.e., they will block
-- until their operations are complete.
class Controller c where

  -- | Lock the device now.
  lock :: MonadIO m => c -> m ()

  -- | Unlock the device now, until a specified date, at which time
  -- the 'Controller' will schedule a lock operation to lock the
  -- device.
  --
  -- If, while an unlock is active, the 'Controller' receives another
  -- unlock request with a later date than the active unlock, the new
  -- request with the later date will take precedence, and the earlier
  -- request's scheduled lock will be canceled or ignored.
  unlock :: MonadIO m => c -> UTCTime -> m ()

  -- | Tell the 'Controller' to quit. This will release any resources
  -- owned by the 'Controller' and its 'Lock' devcie.
  quit :: MonadIO m => c -> m ()
