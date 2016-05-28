-- | Automatically generate client functions for the
-- 'Mellon.Server.API.MellonAPI'.

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Client
         ( -- * Re-exported for convenience.
           State(..)
         , Time(..)
           -- * Client functions
         , clientAPI
         , getTime
         , getState
         , putState
         ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy
import Network.HTTP.Client (Manager)
import Mellon.Server (MellonAPI, State(..), Time(..))
import Servant.API
import Servant.Client

-- | The client API.
clientAPI :: Proxy MellonAPI
clientAPI = Proxy

-- | Generate the client functions for a given host and URL
-- scheme.
getTime :: Manager -> BaseUrl -> ExceptT ServantError IO Time
getState :: Manager -> BaseUrl -> ExceptT ServantError IO State
putState :: State -> Manager -> BaseUrl -> ExceptT ServantError IO State
getTime :<|> getState :<|> putState = client clientAPI
