-- | A @mellon@ controller monad.

module Mellon.MonadController
         ( -- * Classes
           module Mellon.MonadController.Class
           -- * A concurrent controller implementation
         , module Mellon.ConcurrentController
         ) where

import Mellon.MonadController.Class
import Mellon.ConcurrentController
