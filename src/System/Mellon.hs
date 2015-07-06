-- | The top-level module re-exports the modules that a typical @mellon@
-- application will need to interact with an access device.
--
-- However, at the very least, you will also need a useful
-- 'System.Mellon.Lock.Lock' implementation; @System.Mellon@ does not
-- provide one.

module System.Mellon
  ( -- * The generic controller interface.
    module System.Mellon.Controller
    -- * Several 'System.Mellon.Controller.Controller' implementations.
  , module System.Mellon.Impl
  ) where

import System.Mellon.Controller
import System.Mellon.Impl
