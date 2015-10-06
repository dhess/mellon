-- | Top-level @mellon-server@ module, which simply exports the
-- commonly-used sub-modules.
--
-- Note that the "Mellon.Server.Docs" API is not exported here because
-- some of its names conflict with the standard "Mellon.Server.API"
-- module. To create a server that provides a documentation resource,
-- import "Mellon.Server.Docs" directly.

module Mellon.Server
         ( -- * The standard @mellon-server@ API and services.
           module Mellon.Server.API
         ) where

import Mellon.Server.API
