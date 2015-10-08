-- | Automatically generate client functions for the
-- 'Mellon.Server.API.MellonAPI'.

module Mellon.Client
         ( -- * Re-exported for convenience.
           State(..)
         , Time(..)
           -- * Client functions
         , clientAPI
         , generateClientFunctions
         ) where

import Data.Proxy
import Mellon.Server (MellonAPI, State(..), Time(..))
import Servant.Client

-- | The client API.
clientAPI :: Proxy MellonAPI
clientAPI = Proxy

-- | Generate the client functions for a given host and URL
-- scheme.
--
-- > getTime :: EitherT ServantError IO Time
-- > getState :: EitherT ServantError IO State
-- > putState :: State -> EitherT ServantError IO State
-- >
-- > getTime :<|> getState :<|> putState = generateClientFunctions host
-- >   where host = (BaseUrl Http "localhost" 8081)
generateClientFunctions :: BaseUrl -> Client MellonAPI
generateClientFunctions host = client clientAPI host
