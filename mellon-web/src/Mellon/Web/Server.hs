{-|
Module      : Mellon.Web.Server
Description : Top-level server re-exports
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module re-exports the @mellon-web@ server modules.

-}

module Mellon.Web.Server
         ( -- * The standard server API
           module Mellon.Web.Server.API
           -- * The standard server API with self-hosted documentation
         , module Mellon.Web.Server.DocsAPI
         ) where

import Mellon.Web.Server.API
import Mellon.Web.Server.DocsAPI
