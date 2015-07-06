-- | 'Controller' is an abstract typeclass whose instances combine a
-- 'System.Mellon.Lock.Lock' instance with a scheduler implementation,
-- providing the user-facing interface to the @mellon@ state machine
-- model. The user sends 'lock' and 'unlock' commands to a
-- 'Controller' instance, and the 'Controller' interacts with
-- 'System.Mellon.StateMachine.StateMachine' to execute the commands.

module System.Mellon.Controller
       ( Controller(..)
       ) where

import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime)

-- | The primary @mellon@ user-facing interface.
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
  -- owned by the 'Controller' instance and its 'Lock.Lock' device.
  --
  -- After invoking this function, the 'Controller' instance is no
  -- longer valid.
  quit :: MonadIO m => c -> m ()
