{-|
Module      : Mellon.Web.Client
Description : Client actions for the REST web service
Copyright   : (c) 2016, Drew Hess
License     : BSD3
Maintainer  : Drew Hess <src@drewhess.com>
Stability   : experimental
Portability : non-portable

This module provides client-side actions for interacting with the
server-side 'MellonAPI'.

The client actions are implemented on top of the "Network.HTTP.Client"
and "Servant.Client" modules.

-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Mellon.Web.Client
         ( -- * Client actions
           --
           -- | These actions take a 'Manager' and a 'BaseUrl', and
           -- should then be run in an 'ExceptT' transformer stack to
           -- produce a result. For example, assuming the service
           -- endpoint is @http://localhost:8081/@:
           --
           -- > > let baseUrl = BaseUrl Http "localhost" 8081 ""
           -- > > manager <- newManager defaultManagerSettings
           -- > > runExceptT $ putState Locked manager baseUrl
           -- > Right Locked
           getTime
         , getState
         , putState

           -- * Server types
           --
           -- | Re-exported for convenience.
         , State(..)
         , Time(..)
         ) where

import Control.Monad.Trans.Except (ExceptT)
import Data.Proxy (Proxy(..))
import Network.HTTP.Client (Manager)
import Servant.API ((:<|>)(..))
import Servant.Client (BaseUrl, ServantError, client)

import Mellon.Web.Server (MellonAPI, State(..), Time(..))

-- | The client API.
clientAPI :: Proxy MellonAPI
clientAPI = Proxy

-- | Get the server's time. This action is provided chiefly to verify
-- the accuracy of the server's clock.
getTime :: Manager -> BaseUrl -> ExceptT ServantError IO Time

-- | Get the current state of the server's
-- 'Mellon.Controller.Controller'.
getState :: Manager -> BaseUrl -> ExceptT ServantError IO State

-- | Lock or unlock the server's 'Mellon.Controller.Controller'.
putState :: State -> Manager -> BaseUrl -> ExceptT ServantError IO State

getTime :<|> getState :<|> putState = client clientAPI
