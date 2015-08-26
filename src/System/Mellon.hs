-- | The top-level module re-exports the modules that a typical @mellon@
-- application will need to interact with an access device.

module System.Mellon
  ( module System.Mellon.Controller
  , module System.Mellon.MonadLock
  , module System.Mellon.StateMachine
  ) where

import System.Mellon.Controller
import System.Mellon.MonadLock
import System.Mellon.StateMachine
