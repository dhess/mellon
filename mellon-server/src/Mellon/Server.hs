-- | Top-level @mellon-server@ module, which simply exports the
-- sub-modules.

module Mellon.Server
         ( -- * The standard @mellon-server@ API and services.
           module Mellon.Server.API
           -- * Extends the standard API with self-hosted documentation.
         , module Mellon.Server.Docs
         ) where

import Mellon.Server.API
import Mellon.Server.Docs
