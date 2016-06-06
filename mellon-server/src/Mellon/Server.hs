{-|
Module      : Mellon.Server
Description : Top-level re-exports
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

The top-level @mellon-server@ module, which simply exports the
sub-modules.

-}

module Mellon.Server
         ( -- * The standard API
           module Mellon.Server.API
           -- * The standard API with self-hosted documentation
         , module Mellon.Server.DocsAPI
         ) where

import Mellon.Server.API
import Mellon.Server.DocsAPI
