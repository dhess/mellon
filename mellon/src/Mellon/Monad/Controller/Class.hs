-- | A typeclass for controllers.
--
-- Controllers provide the bridge between physical locks and the
-- @mellon@ state machine. 'MonadController' defines the common
-- controller interface implemented by all controllers.

module Mellon.Monad.Controller.Class
       ( MonadController(..)
       -- * Re-exported for convenience.
       , State(..)
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
import Data.Time (UTCTime)
import Mellon.Monad.Controller.Trans (ControllerT(..))
import qualified Mellon.Monad.Controller.Trans as Controller (lockNow, state, unlockUntil)
import Mellon.Monad.StateMachine (State(..))

-- | A controller monad interface.
class (Monad m) => MonadController m where
  -- | Lock the controlled device immediately. Any previously-executed unlock
  -- command that is currently in effect will be canceled.
  lockNow :: m State
  -- | Unlock the controlled device until a given 'UTCTime', at which
  -- time the controller will automatically lock the device again.
  -- Note that you can unlock the device indefinitely by specifying a
  -- time in the past.
  unlockUntil :: UTCTime -> m State
  -- | Get the current state of the controller.
  state :: m State

instance (MonadIO m) => MonadController (ControllerT m) where
  lockNow = Controller.lockNow
  unlockUntil = Controller.unlockUntil
  state = Controller.state

instance (MonadController m) => MonadController (IdentityT m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (SL.StateT s m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (SS.StateT s m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m, Monoid w) => MonadController (WL.WriterT w m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m, Monoid w) => MonadController (WS.WriterT w m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (ReaderT r m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m, Monoid w) => MonadController (RWSL.RWST r w s m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m, Monoid w) => MonadController (RWSS.RWST r w s m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (ExceptT e m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (MaybeT m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (ContT r m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state

instance (MonadController m) => MonadController (ListT m) where
  lockNow = lift lockNow
  unlockUntil = lift . unlockUntil
  state = lift state
