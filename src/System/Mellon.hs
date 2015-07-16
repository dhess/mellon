-- | The top-level module re-exports the modules that a typical @mellon@
-- application will need to interact with an access device.
--
-- However, at the very least, you will also need a useful
-- 'System.Mellon.Lock.Lock' implementation; @System.Mellon@ does not
-- provide one.

module System.Mellon
  ( module System.Mellon.Controller
  , module System.Mellon.StateMachine
  ) where

import System.Mellon.Controller
import System.Mellon.StateMachine
